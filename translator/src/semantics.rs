use dis_xtensa_lx6::Instruction;
use falcon::error::*;
use falcon::il::*;

use super::Mcu;

pub fn imm12_constant(v: u16) -> Constant {
    Constant::new(u64::from(v), 12)
}

pub fn ar_register(r: u8) -> Scalar {
    Scalar::new(format!("AR[{}]", r), 32)
}

macro_rules! windowcheck {
    ($control_flow_graph:expr, $wr:expr, $ws:expr, $wt:expr) => {{
        // TODO: use Mcu::window_base_size

        // (wr ≠ 2'b00 or wt ≠ 2'b00 or wt ≠ 2'b00) and trun(1, WindowStart>>(WindowBase + 1))
        let first_condition = Expression::and(
            Expression::or(
                Expression::cmpneq($wr, expr_const(0, 2))?,
                Expression::or(
                    Expression::cmpneq($ws, expr_const(0, 2))?,
                    Expression::cmpneq($wt, expr_const(0, 2))?,
                )?,
            )?,
            Expression::trun(
                1,
                Expression::shr(
                    expr_scalar("WindowStart", 4),
                    Expression::add(
                        expr_scalar("WindowBase", 4),
                        expr_const(1, 4)
                    )?,
                )?,
            )?,
        )?;
        let first_value = expr_const(0b01, 2);

        // trunc(1, (w >> 1) & 2'b01)
        fn bit1(w: Expression) -> Result<Expression> {
            Expression::trun(
                1,
                Expression::and(
                    Expression::shr(w, expr_const(1, 2))?,
                    expr_const(0b01, 2),
                )?,
            )
        }

        // (wr₁ or ws₁ or wt₁) and trun(1, WindowStart>>(WindowBase + 2))
        let second_condition = Expression::and(
            Expression::or(
                bit1($wr)?,
                Expression::or(
                    bit1($ws)?,
                    bit1($wt)?,
                )?,
            )?,
            Expression::trun(
                1,
                Expression::shr(
                    expr_scalar("WindowStart", 4),
                    Expression::add(
                        expr_scalar("WindowBase", 4),
                        expr_const(2, 4)
                    )?,
                )?,
            )?,
        )?;
        let second_value = expr_const(0b10, 2);

        // (wr = 2'b11 or ws = 2'b11 or wt = 2'b11) and trun(1, WindowStart>>(WindowBase + 3))
        let third_condition = Expression::and(
            Expression::or(
                Expression::cmpeq($wr, expr_const(0b11, 2))?,
                Expression::or(
                    Expression::cmpeq($ws, expr_const(0b11, 2))?,
                    Expression::cmpeq($wt, expr_const(0b11, 2))?,
                )?,
            )?,
            Expression::trun(
                1,
                Expression::shr(
                    expr_scalar("WindowStart", 4),
                    Expression::add(
                        expr_scalar("WindowBase", 4),
                        expr_const(3, 4)
                    )?,
                )?,
            )?,
        )?;
        let third_value = expr_const(0b11, 2);

        let n_expr = Expression::ite(
            first_condition,
            first_value,
            Expression::ite(
                second_condition,
                second_value,
                Expression::ite(
                    third_condition,
                    third_value,
                    expr_const(0b00, 2),
                )?,
            )?,
        )?;

        // TODO: shouldn't be a temp scalar?
        let n = Scalar::new("n", 2);

        let head_index = {
            let block = $control_flow_graph.new_block()?;

            block.assign(n.clone(), n_expr);

            block.index()
        };

        let operation_index = {
            let block = $control_flow_graph.new_block()?;

            let m = Scalar::new("m", 4);
            block.assign(Scalar::new("PS.OWB", 4), expr_scalar("WindowBase", 4));
            block.assign(
                m.clone(),
                Expression::add(
                    expr_scalar("WindowBase", 4),
                    Expression::zext(4, n.clone().into())?,
                )?,
            );
            block.assign(Scalar::new("PS.EXCM", 4), expr_const(1, 4));
            block.assign(Scalar::new("EPC[1]", 32), expr_scalar("PC", 32));
            block.assign(
                Scalar::new("nextPC", 32),
                Expression::ite(
                    Expression::trun(
                        1,
                        Expression::shr(
                            expr_scalar("WindowStart", 4),
                            Expression::add(
                                m.clone().into(),
                                expr_const(1, 4),
                            )?,
                        )?,
                    )?,
                    expr_scalar("WindowOverflow4", 32),
                    Expression::ite(
                        Expression::trun(
                            1,
                            Expression::shr(
                                expr_scalar("WindowStart", 4),
                                Expression::add(
                                    m.clone().into(),
                                    expr_const(2, 4),
                                )?,
                            )?,
                        )?,
                        expr_scalar("WindowOverflow8", 32),
                        expr_scalar("WindowOverflow12", 32),
                    )?,
                )?,
            );

            block.index()
        };

        let condition = Expression::and(
            Expression::cmpeq(
                expr_scalar("CWOE", 2),
                expr_const(1, 2),
            )?,
            Expression::cmpneq(
                n.clone().into(),
                expr_const(0, 2),
            )?,
        )?;

        $control_flow_graph.conditional_edge(
            head_index,
            operation_index,
            condition,
        )?;

        (head_index, operation_index)
    }}
}

pub fn ill(control_flow_graph: &mut ControlFlowGraph) -> Result<()> {
    let block_index = {
        let block = control_flow_graph.new_block()?;

        block.intrinsic(Intrinsic::new(
            "IllegalInstruction",
            "IllegalInstruction",
            Vec::new(),
            None,
            None,
            vec![0, 0, 0],
        ));

        block.index()
    };

    control_flow_graph.set_entry(block_index)?;
    control_flow_graph.set_exit(block_index)?;

    Ok(())
}

pub fn entry(control_flow_graph: &mut ControlFlowGraph, instruction: &Instruction, mcu: &Mcu) -> Result<()> {
    let bri12 = instruction.to_bri12();
    let imm12 = imm12_constant(bri12.imm12);

    let (wcheck_head, wcheck_op) = windowcheck!(
        control_flow_graph,
        expr_const(0, 2),
        expr_scalar("PS.CALLINC", 2),
        expr_const(0, 2)
    );

    let head_index = {
        let block = control_flow_graph.new_block()?;

        block.nop();

        block.index()
    };

    control_flow_graph.unconditional_edge(wcheck_head, head_index)?;
    control_flow_graph.unconditional_edge(wcheck_op, head_index)?;

    let raise_index = {
        let block = control_flow_graph.new_block()?;

        block.intrinsic(Intrinsic::new(
            "IllegalInstruction",
            "IllegalInstruction",
            Vec::new(),
            None,
            None,
            vec![0, 0, 0],
        ));

        block.index()
    };

    let terminating_index = { control_flow_graph.new_block()?.index() };

    // TODO: WindowCheck

    let operation_index = {
        let block = control_flow_graph.new_block()?;

        let ar = ar_register(bri12.s);
        let new_value = Expression::sub(
            ar.clone().into(),
            Expression::shl(
                Expression::zext(32, imm12.into())?,
                Constant::new(3, 32).into(),
            )?,
        )?;
        block.assign(ar, new_value);

        let callinc = scalar("PS.CALLINC", 2);
        let window_base = scalar("WindowBase", mcu.window_base_size());
        let window_base_value = Expression::add(
            window_base.clone().into(),
            Expression::zext(4, callinc.into())?,
        )?;
        block.assign(window_base, window_base_value);

        block.index()
    };

    let condition = Expression::or(
        Expression::cmpltu(
            expr_const(3, 4),
            expr_const(bri12.s as u64, 4),
        )?,
        Expression::or(
            Expression::cmpeq(
                expr_scalar("PS.WOE", 1),
                expr_const(0, 1),
            )?,
            Expression::cmpeq(
                expr_scalar("PS.EXCM", 4),
                expr_const(1, 4),
            )?,
        )?,
    )?;

    control_flow_graph.conditional_edge(
        head_index,
        raise_index,
        condition.clone(),
    )?;

    control_flow_graph.conditional_edge(
        head_index,
        operation_index,
        Expression::cmpeq(condition, expr_const(0, 1))?,
    )?;

    control_flow_graph.unconditional_edge(raise_index, terminating_index)?;
    control_flow_graph.unconditional_edge(operation_index, terminating_index)?;

    Ok(())
}

#[cfg(test)]
pub mod tests {
    use super::*;

    use falcon::il::*;
    use falcon::graph::*;

    use dis_xtensa_lx6;

    use super::super::Mcu;

    #[test]
    fn dot_graph() {
        let op = (0x36, 0x41, 0x00);
        let instruction = dis_xtensa_lx6::match_opcode(op).unwrap();
        assert_eq!(instruction.id, dis_xtensa_lx6::Id::ENTRY);

        let mut instruction_graph = ControlFlowGraph::new();

        let mcu = Mcu::esp32();

        if let Err(e) = entry(&mut instruction_graph, &instruction, &mcu) {
            println!("{:?}", e);
            panic!();
        }

        println!("{}", instruction_graph.graph().dot_graph());
        panic!();
    }
}
