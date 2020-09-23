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

#[derive(Debug, Clone)]
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

fn compile(instructions: &[Instruction]) -> Box<dyn '_ + Fn(&mut Vec<u8>, usize) -> usize> {
    if instructions.is_empty() {
        return Box::new(move |_tape, p| p);
    }

    let rest: Box<dyn '_ + Fn(&mut Vec<u8>, usize) -> usize> = compile(&instructions[1..]);
    match &instructions[0] {
        Instruction::IncrementPointer => Box::new(move |tape, p| rest(tape, p + 1)),
        Instruction::DecrementPointer => Box::new(move |tape, p| rest(tape, p - 1)),
        Instruction::Increment => Box::new(move |tape, p| {
            tape[p] += 1;
            rest(tape, p)
        }),
        Instruction::Decrement => Box::new(move |tape, p| {
            tape[p] -= 1;
            rest(tape, p)
        }),
        Instruction::Write => Box::new(move |tape, p| {
            print!("{}", tape[p] as char);
            rest(tape, p)
        }),
        Instruction::Read => Box::new(move |tape, p| {
            let mut input: [u8; 1] = [0; 1];
            std::io::stdin()
                .read_exact(&mut input)
                .expect("failed to read stdin");
            tape[p] = input[0];
            rest(tape, p)
        }),
        Instruction::Loop(nested_instructions) => {
            let inner = compile(&nested_instructions);
            Box::new(move |tape, mut p| {
                while tape[p] != 0 {
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
    let code = compile(&program);
    code(&mut tape, data_pointer);
}
