use anyhow::Result;
use clap::{crate_description, crate_name, crate_version, App, Arg};
use log::{debug, LevelFilter};
use std::fs;
use std::mem;

use whiteye::ast::Ast;
use whiteye::machine::Machine;
use whiteye::parser::parse;

fn main() -> Result<()> {
    let app = App::new(crate_name!())
        .about(crate_description!())
        .version(crate_version!())
        .arg(Arg::with_name("FILE"))
        .arg(Arg::with_name("dbg").short("d").long("debug"));

    let matches = app.get_matches();

    let is_debug = matches.occurrences_of("dbg") != 0;

    let mut logger = env_logger::Builder::new();

    if is_debug {
        logger.filter_level(LevelFilter::Debug);
    } else {
        logger.filter_level(LevelFilter::Info);
    }

    logger.init();

    let mut jit = whiteye::jit::JIT::default();

    let code_ptr = jit.compile([Ast::Number(1)].to_vec()).unwrap();

    jit.create_data("hello_string", "hello world!\0".as_bytes().to_vec())
        .unwrap();

    unsafe {
        let code_fn = mem::transmute::<_, fn(()) -> ()>(code_ptr);
        println!("{:?}", code_fn(()));
    }

    if let Some(path) = matches.value_of("FILE") {
        let input = fs::read_to_string(path)?;

        let mut machine = Machine::new();

        debug!("Raw: \n{}", input);

        let parsed = parse(&input).unwrap_or_else(|e| panic!("{}", e));
        debug!("AST: {:?}", parsed);

        for ast in parsed {
            machine.run(ast).unwrap_or_else(|e| panic!("{}", e));
            debug!("machine state: {:?}", machine);
        }

        /*
        if !input.is_empty() {
            eprintln!("parsing error, input remaining {:?}", input);
            exit(1);
        }
        */
    }

    Ok(())
}
