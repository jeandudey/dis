use falcon::error::*;
use falcon::il::*;

pub fn ill(control_flow_graph: &mut ControlFlowGraph) -> Result<()> {
    let raise_index = {
        let block = control_flow_graph.new_block()?;

        block.intrinsic(Intrinsic::new(
            "Exception(IllegalInstructionCause)",
            "Exception(IllegalInstructionCause)",
            Vec::new(),
            None,
            None,
            vec![0, 0, 0],
        ));

        block.index()
    };

    let terminating_index = { control_flow_graph.new_block()?.index() };

    control_flow_graph.unconditional_edge(raise_index, terminating_index)?;

    control_flow_graph.set_entry(raise_index)?;
    control_flow_graph.set_exit(terminating_index)?;

    Ok(())
}
