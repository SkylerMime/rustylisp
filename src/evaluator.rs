use crate::lexer::{AstNumber, Binary, FuncType, NAry, NoArgs, Unary};
use crate::parser::{AstFunction, AstNode};

pub fn eval(root: &AstNode) -> Result<AstNumber, String> {
    eval_s_expr(root)
}

fn eval_s_expr(s_expr: &AstNode) -> Result<AstNumber, String> {
    match s_expr {
        AstNode::FuncNode(function) => eval_function(function),
        AstNode::NumNode(number) => Ok(number.clone()),
    }
}

fn eval_function(function: &AstFunction) -> Result<AstNumber, String> {
    match function.func.clone() {
        FuncType::NAry(func_type) => {
            // TODO: Rework without extra clone / multiple streams
            let maybe_operands = function.operands.iter().map(|op| eval_s_expr(op));
            if maybe_operands.clone().any(|evaluation| evaluation.is_err()) {
                return Err(maybe_operands
                    .filter(|evaluation| evaluation.is_err())
                    .map(|err| err.unwrap_err())
                    .collect());
            };
            Ok(maybe_operands
                .map(|some_num| some_num.expect("These should all be 'Some' entries."))
                .reduce(|a, b| match func_type {
                    NAry::Add => add(a, b),
                    NAry::Mult => mult(a, b),
                    NAry::Max => max(a, b),
                    NAry::Min => min(a, b),
                    NAry::Hypot => hypot(a, b),
                })
                .unwrap_or(func_type.no_args()))
        }
        FuncType::Binary(func_type) => {
            if let Some(first_arg) = function.operands.get(0) {
                if let Some(second_arg) = function.operands.get(1) {
                    let first_evaluated = eval_s_expr(first_arg);
                    let second_evaluated = eval_s_expr(second_arg);

                    if let Ok(first) = first_evaluated {
                        if let Ok(second) = second_evaluated {
                            Ok(match func_type {
                                Binary::Sub => sub(first, second),
                                Binary::Div => div(first, second),
                                Binary::Pow => pow(first, second),
                                Binary::Remainder => remainder(first, second),
                            })
                        } else {
                            // Send up the error message
                            second_evaluated
                        }
                    } else {
                        // Send up the error message
                        first_evaluated
                    }
                } else {
                    Err(String::from("Binary function requires two arguments"))
                }
            } else {
                Err(String::from("The arguments should not be empty"))
            }
        }
        FuncType::Unary(func_type) => {
            if let Some(operand) = function.operands.get(0) {
                let evaluated = eval_s_expr(operand);
                if let Ok(result) = evaluated {
                    Ok(match func_type {
                        Unary::Abs => abs(result),
                        Unary::Exp => exp(result),
                        Unary::Exp2 => exp2(result),
                        Unary::Log => log(result),
                        Unary::Sqrt => sqrt(result),
                        Unary::Cbrt => cbrt(result),
                        Unary::Neg => neg(result),
                    })
                } else {
                    evaluated
                }
            } else {
                return Err(String::from("The arguments should not be empty"));
            }
        }
    }
}

fn neg(a: AstNumber) -> AstNumber {
    match a {
        AstNumber::Int(value) => AstNumber::Int(value * -1),
        AstNumber::Double(value) => AstNumber::Double(value * -1.0),
        AstNumber::NAN => AstNumber::NAN,
    }
}

fn abs(a: AstNumber) -> AstNumber {
    match a {
        AstNumber::Int(value) if value < 0 => AstNumber::Int(value * -1),
        AstNumber::Double(value) if value < 0.0 => AstNumber::Double(value * -1.0),
        _ => a,
    }
}

fn exp(a: AstNumber) -> AstNumber {
    coerce_to_double_unary(a, |u| u.exp())
}

fn exp2(a: AstNumber) -> AstNumber {
    match a {
        AstNumber::Int(value) if value >= 0 => AstNumber::Int((2 as i32).pow(value as u32)),
        AstNumber::Int(value) => AstNumber::Double((2.0 as f32).powf(value as f32)),
        AstNumber::Double(value) => AstNumber::Double((2.0 as f32).powf(value as f32)),
        AstNumber::NAN => AstNumber::NAN,
    }
}

fn log(a: AstNumber) -> AstNumber {
    coerce_to_double_unary(a, |u| u.ln())
}

fn sqrt(a: AstNumber) -> AstNumber {
    coerce_to_double_unary(a, |u| u.sqrt())
}

fn cbrt(a: AstNumber) -> AstNumber {
    coerce_to_double_unary(a, |u| u.cbrt())
}

fn coerce_to_double_unary(a: AstNumber, double_operation: fn(f32) -> f32) -> AstNumber {
    match a {
        AstNumber::Int(value) => AstNumber::Double(double_operation(value as f32)),
        AstNumber::Double(value) => AstNumber::Double(double_operation(value)),
        AstNumber::NAN => AstNumber::NAN,
    }
}

fn add(a: AstNumber, b: AstNumber) -> AstNumber {
    apply_with_case(a, b, |u, v| -> f32 { u + v }, |u, v| -> i32 { u + v })
}

fn mult(a: AstNumber, b: AstNumber) -> AstNumber {
    apply_with_case(a, b, |u, v| -> f32 { u * v }, |u, v| -> i32 { u * v })
}

fn max(a: AstNumber, b: AstNumber) -> AstNumber {
    compare_and_keep_winner(a, b, |u, v| -> f32 { u.max(v) })
}

fn min(a: AstNumber, b: AstNumber) -> AstNumber {
    compare_and_keep_winner(a, b, |u, v| -> f32 { u.min(v) })
}

fn hypot(a: AstNumber, b: AstNumber) -> AstNumber {
    coerce_to_double_binary(a, b, |u, v| -> f32 { u.hypot(v) })
}

fn sub(a: AstNumber, b: AstNumber) -> AstNumber {
    apply_with_case(a, b, |u, v| -> f32 { u - v }, |u, v| -> i32 { u - v })
}

fn div(a: AstNumber, b: AstNumber) -> AstNumber {
    apply_with_case(a, b, |u, v| -> f32 { u / v }, |u, v| -> i32 { u / v })
}

fn remainder(a: AstNumber, b: AstNumber) -> AstNumber {
    apply_with_case(a, b, |u, v| -> f32 { u % v }, |u, v| -> i32 { u % v })
}

/// Panics if b is negative
fn pow(a: AstNumber, b: AstNumber) -> AstNumber {
    // TODO: Prevent panic
    apply_with_case(
        a,
        b,
        |u, v| -> f32 { u.powf(v) },
        |u, v| -> i32 {
            u.pow(
                v.try_into()
                    .expect("Second argument should not be negative"),
            )
        },
    )
}

fn coerce_to_double_binary(
    a: AstNumber,
    b: AstNumber,
    double_operation: fn(f32, f32) -> f32,
) -> AstNumber {
    match (a, b) {
        (AstNumber::Int(first), AstNumber::Int(second)) => {
            AstNumber::Double(double_operation(first as f32, second as f32))
        }
        (AstNumber::Int(first), AstNumber::Double(second)) => {
            AstNumber::Double(double_operation(first as f32, second))
        }
        (AstNumber::Double(first), AstNumber::Int(second)) => {
            AstNumber::Double(double_operation(first, second as f32))
        }
        (AstNumber::Double(first), AstNumber::Double(second)) => {
            AstNumber::Double(double_operation(first, second))
        }
        _ => AstNumber::NAN,
    }
}

fn compare_and_keep_winner(
    a: AstNumber,
    b: AstNumber,
    comparator: fn(f32, f32) -> f32,
) -> AstNumber {
    match (a, b) {
        (AstNumber::Int(first), AstNumber::Int(second)) => {
            AstNumber::Int(comparator(first as f32, second as f32) as i32)
        }
        (AstNumber::Int(first), AstNumber::Double(second)) => {
            let winner = comparator(first as f32, second as f32);
            if winner == first as f32 {
                AstNumber::Int(first)
            } else {
                AstNumber::Double(second)
            }
        }
        (AstNumber::Double(first), AstNumber::Int(second)) => {
            let winner = comparator(first as f32, second as f32);
            if winner == first {
                AstNumber::Double(first)
            } else {
                AstNumber::Int(second)
            }
        }
        (AstNumber::Double(first), AstNumber::Double(second)) => {
            AstNumber::Double(comparator(first as f32, second as f32))
        }
        _ => AstNumber::NAN,
    }
}

fn apply_with_case(
    a: AstNumber,
    b: AstNumber,
    double_operation: fn(f32, f32) -> f32,
    int_operation: fn(i32, i32) -> i32,
) -> AstNumber {
    match (a, b) {
        (AstNumber::Int(first), AstNumber::Int(second)) => {
            AstNumber::Int(int_operation(first, second))
        }
        (AstNumber::Int(first), AstNumber::Double(second)) => {
            AstNumber::Double(double_operation(first as f32, second))
        }
        (AstNumber::Double(first), AstNumber::Int(second)) => {
            AstNumber::Double(double_operation(first, second as f32))
        }
        (AstNumber::Double(first), AstNumber::Double(second)) => {
            AstNumber::Double(double_operation(first, second))
        }
        _ => AstNumber::NAN,
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        evaluator::eval,
        lexer::{
            AstNumber::{self, *},
            Binary::*,
            FuncType::*,
            NAry::*,
            Unary::*,
        },
        parser::{AstFunction, AstNode::*},
    };

    #[test]
    fn it_evaluates_add_function() {
        let add_tree = FuncNode(AstFunction {
            func: NAry(Add),
            operands: vec![NumNode(Double(1.5)), NumNode(Int(8)), NumNode(Int(2))],
        });

        assert_eq!(eval(&add_tree), Ok(Double(11.5)))
    }

    #[test]
    fn add_no_args_returns_zero() {
        let add_tree = FuncNode(AstFunction {
            func: NAry(Add),
            operands: Vec::new(),
        });

        assert_eq!(eval(&add_tree), Ok(Int(0)))
    }

    #[test]
    fn it_evaluates_mult_function() {
        let mult_tree = FuncNode(AstFunction {
            func: NAry(Mult),
            operands: vec![NumNode(Double(0.5)), NumNode(Int(8)), NumNode(Int(2))],
        });

        assert_eq!(eval(&mult_tree), Ok(Double(8.0)))
    }

    #[test]
    fn mult_no_args_returns_one() {
        let mult_tree = FuncNode(AstFunction {
            func: NAry(Mult),
            operands: Vec::new(),
        });

        assert_eq!(eval(&mult_tree), Ok(Int(1)))
    }

    #[test]
    fn it_evaluates_max_function() {
        let max_tree = FuncNode(AstFunction {
            func: NAry(Max),
            operands: vec![NumNode(Double(0.5)), NumNode(Int(-100)), NumNode(Int(2))],
        });

        assert_eq!(eval(&max_tree), Ok(Int(2)))
    }

    #[test]
    fn max_no_args_returns_nan() {
        let max_tree = FuncNode(AstFunction {
            func: NAry(Max),
            operands: Vec::new(),
        });

        assert_eq!(eval(&max_tree), Ok(NAN))
    }

    #[test]
    fn it_evaluates_min_function() {
        let min_tree = FuncNode(AstFunction {
            func: NAry(Min),
            operands: vec![
                NumNode(Double(0.5)),
                NumNode(Int(-100)),
                NumNode(Int(2)),
                NumNode(Double(-101.5)),
            ],
        });

        assert_eq!(eval(&min_tree), Ok(Double(-101.5)))
    }

    #[test]
    fn min_no_args_returns_nan() {
        let min_tree = FuncNode(AstFunction {
            func: NAry(Min),
            operands: Vec::new(),
        });

        assert_eq!(eval(&min_tree), Ok(NAN))
    }

    #[test]
    fn it_evaluates_hypot_function() {
        let hypot_tree = FuncNode(AstFunction {
            func: NAry(Hypot),
            operands: vec![NumNode(Int(3)), NumNode(Int(4))],
        });

        assert_eq!(eval(&hypot_tree), Ok(Double(5.0)))
    }

    #[test]
    fn hypot_no_args_returns_zero() {
        let hypot_tree = FuncNode(AstFunction {
            func: NAry(Hypot),
            operands: Vec::new(),
        });

        assert_eq!(eval(&hypot_tree), Ok(Double(0.0)))
    }

    #[test]
    fn it_evaluates_sub_function() {
        let sub_tree = FuncNode(AstFunction {
            func: Binary(Sub),
            operands: vec![NumNode(Int(8)), NumNode(Int(2))],
        });

        assert_eq!(eval(&sub_tree), Ok(Int(6)))
    }

    #[test]
    fn it_evaluates_div_function_ints() {
        let div_tree = FuncNode(AstFunction {
            func: Binary(Div),
            operands: vec![NumNode(Int(8)), NumNode(Int(3))],
        });

        // Integer division should floor the result
        assert_eq!(eval(&div_tree), Ok(Int(2)))
    }

    #[test]
    fn it_evaluates_div_function_doubles() {
        let div_tree = FuncNode(AstFunction {
            func: Binary(Div),
            operands: vec![NumNode(Double(-9.0)), NumNode(Int(2))],
        });

        assert_eq!(eval(&div_tree), Ok(Double(-4.5)))
    }

    #[test]
    fn it_evaluates_pow_function() {
        let pow_tree = FuncNode(AstFunction {
            func: Binary(Pow),
            operands: vec![NumNode(Int(5)), NumNode(Int(2))],
        });

        assert_eq!(eval(&pow_tree), Ok(Int(25)))
    }

    #[test]
    fn it_evaluates_mod_function_ints() {
        let mod_tree = FuncNode(AstFunction {
            func: Binary(Remainder),
            operands: vec![NumNode(Int(18)), NumNode(Int(4))],
        });

        assert_eq!(eval(&mod_tree), Ok(Int(2)))
    }

    #[test]
    fn it_evaluates_mod_function_doubles() {
        let mod_tree = FuncNode(AstFunction {
            func: Binary(Remainder),
            operands: vec![NumNode(Double(5.5)), NumNode(Double(2.1))],
        });

        assert!(almost_equal(eval(&mod_tree), 1.3));
    }

    fn almost_equal(actual: Result<AstNumber, String>, expected: f32) -> bool {
        if let Ok(Double(actual_value)) = actual {
            if (expected - actual_value).abs() < 0.001 {
                return true;
            } else {
                println!("actual: {}", actual_value);
                return false;
            }
        } else {
            panic!("Not an ok double");
        }
    }

    #[test]
    fn it_evaluates_abs_function() {
        let sub_tree = FuncNode(AstFunction {
            func: Unary(Abs),
            operands: vec![NumNode(Int(-8))],
        });

        assert_eq!(eval(&sub_tree), Ok(Int(8)))
    }

    #[test]
    fn it_evaluates_exp_function() {
        let exp_tree = FuncNode(AstFunction {
            func: Unary(Exp),
            operands: vec![NumNode(Int(2))],
        });

        assert!(almost_equal(eval(&exp_tree), 7.38905))
    }

    #[test]
    fn it_evaluates_ln_function() {
        let exp_tree = FuncNode(AstFunction {
            func: Unary(Log),
            operands: vec![NumNode(Int(2))],
        });

        assert!(almost_equal(eval(&exp_tree), 0.69314))
    }

    #[test]
    fn it_evaluates_sqrt_function() {
        let exp_tree = FuncNode(AstFunction {
            func: Unary(Sqrt),
            operands: vec![NumNode(Double(25.0))],
        });

        assert!(almost_equal(eval(&exp_tree), 5.0))
    }

    #[test]
    fn it_evaluates_cbrt_function() {
        let exp_tree = FuncNode(AstFunction {
            func: Unary(Cbrt),
            operands: vec![NumNode(Int(27))],
        });

        assert!(almost_equal(eval(&exp_tree), 3.0))
    }

    #[test]
    fn it_evaluates_exp2_function_ints() {
        let exp_tree = FuncNode(AstFunction {
            func: Unary(Exp2),
            operands: vec![NumNode(Int(3))],
        });

        assert_eq!(eval(&exp_tree), Ok(Int(8)))
    }

    #[test]
    fn it_evaluates_exp2_function_doubles() {
        let exp_tree = FuncNode(AstFunction {
            func: Unary(Exp2),
            operands: vec![NumNode(Double(0.5))],
        });

        assert!(almost_equal(eval(&exp_tree), 1.4142135))
    }
}
