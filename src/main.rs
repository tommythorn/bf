use std::env;
use std::fs::File;
use std::io::Read;

/// Opcodes determined by the lexer
#[derive(Debug, Clone)]
enum OpCode {
    IncrementPointer,
    DecrementPointer,
    Increment,
    Decrement,
    Write,
    Read,
    LoopBegin,
    LoopEnd,
}

#[derive(Debug, Clone, PartialEq)]
enum Instruction {
    IncrementPointer,
    DecrementPointer,
    Increment,
    Decrement,
    Write,
    Read,
    Loop(Vec<Instruction>),
}

/// Lexer turns the source code into a sequence of opcodes
fn lex(source: String) -> Vec<OpCode> {
    let mut operations = Vec::new();

    for symbol in source.chars() {
        let op = match symbol {
            '>' => Some(OpCode::IncrementPointer),
            '<' => Some(OpCode::DecrementPointer),
            '+' => Some(OpCode::Increment),
            '-' => Some(OpCode::Decrement),
            '.' => Some(OpCode::Write),
            ',' => Some(OpCode::Read),
            '[' => Some(OpCode::LoopBegin),
            ']' => Some(OpCode::LoopEnd),
            _ => None,
        };

        // Non-opcode characters are simply comments
        if let Some(op) = op {
            operations.push(op);
        }
    }

    operations
}

fn parse(opcodes: Vec<OpCode>) -> Vec<Instruction> {
    let mut program: Vec<Instruction> = Vec::new();
    let mut loop_stack = 0;
    let mut loop_start = 0;

    for (i, op) in opcodes.iter().enumerate() {
        if loop_stack == 0 {
            let instr = match op {
                OpCode::IncrementPointer => Some(Instruction::IncrementPointer),
                OpCode::DecrementPointer => Some(Instruction::DecrementPointer),
                OpCode::Increment => Some(Instruction::Increment),
                OpCode::Decrement => Some(Instruction::Decrement),
                OpCode::Write => Some(Instruction::Write),
                OpCode::Read => Some(Instruction::Read),

                OpCode::LoopBegin => {
                    loop_start = i;
                    loop_stack += 1;
                    None
                }

                OpCode::LoopEnd => panic!("loop ending at #{} has no beginning", i),
            };

            if let Some(instr) = instr {
                program.push(instr);
            }
        } else {
            match op {
                OpCode::LoopBegin => {
                    loop_stack += 1;
                }
                OpCode::LoopEnd => {
                    loop_stack -= 1;

                    if loop_stack == 0 {
                        program.push(Instruction::Loop(parse(
                            opcodes[loop_start + 1..i].to_vec(),
                        )));
                    }
                }
                _ => (),
            }
        }
    }

    if loop_stack != 0 {
        panic!(
            "loop that starts at #{} has no matching ending!",
            loop_start
        );
    }

    program
}

/*
 * The original instructions are at too-low level so to catch interesting patterns, we'll raise it a bit,
 * rewriting the original stream into bigger instructions.
 */

#[derive(Debug, Clone, PartialEq)]
enum BigInsn {
    Move(i32),
    Adj(i32),
    Write,
    Read,
    Loop(Vec<BigInsn>),
}

fn emit(bigcode: &mut Vec<BigInsn>, deltap: &mut i32, delta: &mut i32) {
    if *deltap != 0 {
        bigcode.push(BigInsn::Move(*deltap));
        *deltap = 0;
    }

    if *delta != 0 {
        bigcode.push(BigInsn::Adj(*delta));
        *delta = 0;
    }
}

fn maybe_emit(bigcode: &mut Vec<BigInsn>, deltap: &mut i32, delta: &mut i32) {
    if *delta != 0 {
        emit(bigcode, deltap, delta);
    }
}

/**
This function translates ('<' | '>')+ ('+' | '-')+ into MoveAdj N M instructions.

the lowlevel BF instructions into the higher-level BigInsn
by abstractly simulating the movement of the < > and + -.
*/
fn raise_abstraction(instructions: &[Instruction]) -> Vec<BigInsn> {
    let mut deltap: i32 = 0;
    let mut delta: i32 = 0;
    let mut bigcode = vec![];

    for insn in instructions.iter() {
        match &insn {
            Instruction::IncrementPointer | Instruction::DecrementPointer => {
                maybe_emit(&mut bigcode, &mut deltap, &mut delta);
                if *insn == Instruction::IncrementPointer {
                    deltap += 1;
                } else {
                    deltap -= 1;
                }
            }
            Instruction::Increment => delta += 1,
            Instruction::Decrement => delta -= 1,
            Instruction::Write => {
                emit(&mut bigcode, &mut deltap, &mut delta);
                bigcode.push(BigInsn::Write);
            }
            Instruction::Read => {
                emit(&mut bigcode, &mut deltap, &mut delta);
                bigcode.push(BigInsn::Read);
            }
            Instruction::Loop(body) => {
                emit(&mut bigcode, &mut deltap, &mut delta);
                bigcode.push(BigInsn::Loop(raise_abstraction(body)));
                assert_eq!(deltap, 0);
                assert_eq!(delta, 0);
            }
        }
    }

    emit(&mut bigcode, &mut deltap, &mut delta);

    bigcode
}

fn compile(
    instructions: &[Instruction],
    delta_p: i32,
) -> Box<dyn '_ + Fn(&mut Vec<u8>, i32) -> i32> {
    if instructions.is_empty() {
        return Box::new(move |_tape, p| p + delta_p);
    }

    match &instructions[0] {
        Instruction::IncrementPointer => compile(&instructions[1..], delta_p + 1),
        Instruction::DecrementPointer => compile(&instructions[1..], delta_p - 1),
        Instruction::Increment => {
            let rest = compile(&instructions[1..], 0);

            Box::new(move |tape, mut p| {
                p += delta_p;
                tape[p as usize] += 1;
                rest(tape, p)
            })
        }
        Instruction::Decrement => {
            let rest = compile(&instructions[1..], 0);

            Box::new(move |tape, mut p| {
                p += delta_p;
                tape[p as usize] -= 1;
                rest(tape, p)
            })
        }
        Instruction::Write => {
            let rest = compile(&instructions[1..], 0);

            Box::new(move |tape, mut p| {
                p += delta_p;
                print!("{}", tape[p as usize] as char);
                rest(tape, p)
            })
        }
        Instruction::Read => {
            let rest = compile(&instructions[1..], 0);

            Box::new(move |tape, mut p| {
                let mut input: [u8; 1] = [0; 1];
                std::io::stdin()
                    .read_exact(&mut input)
                    .expect("failed to read stdin");
                p += delta_p;
                tape[p as usize] = input[0];
                rest(tape, p)
            })
        }

        Instruction::Loop(nested_instructions) => {
            let rest = compile(&instructions[1..], 0);

            if nested_instructions.len() == 1 && nested_instructions[0] == Instruction::Decrement {
                // Special case [-] which sets take[p] to 0
                return Box::new(move |tape, mut p| {
                    p += delta_p;
                    tape[p as usize] = 0;
                    rest(tape, p)
                });
            }

            let inner = compile(&nested_instructions, 0);
            Box::new(move |tape, mut p| {
                p += delta_p;
                while tape[p as usize] != 0 {
                    p = inner(tape, p);
                }
                rest(tape, p)
            })
        }
    }
}

/// Executes a program that was previously parsed
// This is the original code, keeping it here for now
#[allow(dead_code)]
fn run(instructions: &[Instruction], tape: &mut Vec<u8>, data_pointer: &mut usize) {
    for instr in instructions {
        match instr {
            Instruction::IncrementPointer => *data_pointer += 1,
            Instruction::DecrementPointer => *data_pointer -= 1,
            Instruction::Increment => tape[*data_pointer] += 1,
            Instruction::Decrement => tape[*data_pointer] -= 1,
            Instruction::Write => print!("{}", tape[*data_pointer] as char),
            Instruction::Read => {
                let mut input: [u8; 1] = [0; 1];
                std::io::stdin()
                    .read_exact(&mut input)
                    .expect("failed to read stdin");
                tape[*data_pointer] = input[0];
            }
            Instruction::Loop(nested_instructions) => {
                while tape[*data_pointer] != 0 {
                    run(&nested_instructions, tape, data_pointer)
                }
            }
        }
    }
}

fn main() {
    // Determine which file to execute
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        println!("usage: bf <file.bf>");
        std::process::exit(1);
    }

    let filename = &args[1];

    // Read file
    let mut file = File::open(filename).expect("program file not found");
    let mut source = String::new();
    file.read_to_string(&mut source)
        .expect("failed to read program file");

    // Lex file into opcodes
    let opcodes = lex(source);

    // Parse opcodes into program
    let program = parse(opcodes);

    // Set up environment and run program
    let mut tape: Vec<u8> = vec![0; 1024];
    let data_pointer = 512;
    // run(&program, &mut tape, &mut data_pointer);
    println!("{:?}", raise_abstraction(&program));
    let code = compile(&program, 0);
    code(&mut tape, data_pointer);
}
