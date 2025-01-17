use crate::parser::{ConditionBlock, Operator, ParserInstr};
use crate::util::{error, split_vec};
use internment::Intern;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Copy, Serialize, Deserialize)]
#[repr(u8)]
pub enum Instr {
    StopStore,
    Null,
    Store,
    StoreArg,
    Operation(Operator),
    FuncReturn,
    // JUMP X INSTRUCTIONS -- IS_NEGATIVE
    Jump(bool, u16),
    // JUMP SIZE IF CONDITION IS FALSE
    If(u16),

    // VarStore(u32),
    // VarUpdate(u32),
    VarSet(u32),             // index of variable
    FuncCall(u32),           // index of str in str_pool
    VariableIdentifier(u32), // index of str in str_pool

    Bool(bool),
    String(u32),
    Integer(i32),
    Float(f32),
}

fn types_to_instr(x: ParserInstr) -> Instr {
    match x {
        ParserInstr::Integer(int) => return Instr::Integer(int),
        ParserInstr::Float(float) => return Instr::Float(float),
        ParserInstr::Bool(bool) => return Instr::Bool(bool),
        ParserInstr::Operation(op) => return Instr::Operation(op),
        _ => todo!("{:?}", x),
    }
}

#[inline(never)]
pub fn parser_to_instr_set(
    lines: Vec<ParserInstr>,
    store: bool,
    locals: &mut Vec<Intern<String>>,
    variables: &mut Vec<Intern<String>>,
) -> Vec<Instr> {
    let mut output: Vec<Instr> = Vec::with_capacity(lines.len());
    for x in lines {
        match x {
            ParserInstr::VariableDeclaration(block) => {
                let x = block.name;
                let y = block.value;
                let result = parser_to_instr_set(Vec::from(y), true, locals, variables);
                output.extend(result);
                let name = Intern::from(x);
                locals.push(name);
                if block.is_declared {
                    if let Some(index) = variables.iter().position(|id| *id == name) {
                        output.push(Instr::VarSet(index as u32));
                    }
                } else {
                    output.push(Instr::VarSet((locals.len() - 1) as u32));
                    variables.push(name);
                }
            }
            ParserInstr::FunctionCall(block) => {
                let name = block.name;
                let y = block.args;
                for x in split_vec(Vec::from(y), ParserInstr::Separator) {
                    let result = parser_to_instr_set(x, true, locals, variables);
                    output.extend(result);
                    output.push(Instr::StoreArg);
                }
                locals.push(Intern::from(name));
                output.push(Instr::FuncCall((locals.len() - 1) as u32));
            }
            ParserInstr::FunctionReturn(ret) => {
                let result = parser_to_instr_set(Vec::from(ret), true, locals, variables);
                if result != vec![Instr::Store, Instr::StopStore] {
                    output.extend(result);
                }
                output.push(Instr::FuncReturn);
            }
            ParserInstr::Condition(block) => {
                let condition =
                    parser_to_instr_set(Vec::from(block.condition), true, locals, variables);
                let in_code = parser_to_instr_set(Vec::from(block.code), false, locals, variables);
                if condition == vec![Instr::Store, Instr::Bool(true), Instr::StopStore]
                    || condition == vec![Instr::Store, Instr::StopStore]
                {
                    output.extend(in_code);
                    continue;
                }
                output.extend(condition);

                let mut added_else_length = 0;
                let mut added_else_blocks: Vec<Instr> =
                    Vec::with_capacity(block.else_blocks.len() * 5);
                for else_block in block.else_blocks.clone() {
                    let else_condition = else_block.0;
                    let else_condition_length = else_condition.len();

                    if else_condition_length == 0 {
                        let block = ParserInstr::Condition(Box::from(ConditionBlock {
                            condition: else_condition,
                            code: else_block.1,
                            else_blocks: Box::new([]),
                        }));
                        let result = parser_to_instr_set(vec![block], false, locals, variables);

                        added_else_length += result.len();
                        added_else_blocks.extend(result);
                    } else {
                        let mut else_blocks = block.else_blocks.into_vec();
                        else_blocks.remove(0);
                        let block = ParserInstr::Condition(Box::from(ConditionBlock {
                            condition: else_condition,
                            code: else_block.1,
                            else_blocks: Box::from(else_blocks),
                        }));
                        let result = parser_to_instr_set(vec![block], false, locals, variables);
                        added_else_length += result.len();
                        added_else_blocks.extend(result);
                        break;
                    }
                }

                let main_branch_length = in_code.len();
                if added_else_length != 0 {
                    output.push(Instr::If((main_branch_length + 1) as u16));
                    output.extend(in_code);
                    output.push(Instr::Jump(false, added_else_length as u16));
                    output.extend(added_else_blocks)
                } else {
                    output.push(Instr::If((main_branch_length) as u16));
                    output.extend(in_code)
                }
            }
            ParserInstr::While(block) => {
                let condition =
                    parser_to_instr_set(Vec::from(block.condition), true, locals, variables);
                let in_code = parser_to_instr_set(Vec::from(block.code), false, locals, variables);
                let added = in_code.len();
                let sum = condition.len() + 1 + added;
                output.extend(condition);
                output.push(Instr::If((added + 1) as u16));
                output.extend(in_code);
                output.push(Instr::Jump(true, sum as u16))
            }
            ParserInstr::String(str) => {
                locals.push(Intern::from(str));
                output.push(Instr::String((locals.len() - 1) as u32));
            }
            ParserInstr::VariableIdentifier(str) => {
                let name = Intern::from(str);
                if let Some(index) = variables.iter().position(|id| *id == name) {
                    locals.push(name);
                    output.push(Instr::VariableIdentifier(index as u32));
                } else {
                    error(&format!("Unknown variable {name}"), "");
                }
            }
            _ => output.push(types_to_instr(x)),
        }
    }
    if store {
        output.insert(0, Instr::Store);
        output.push(Instr::StopStore)
    }
    output
}
