use std::{
    env,
    fs::File,
    io::{self, BufReader, Read, Write},
    path::Path,
};

use myvm::{binfmt::BinaryFile, pp_macros, preprocessor::PreProcessor};
use strum_macros::Display;

#[derive(Debug, PartialEq, Eq, Display)]
enum ArgsError {
    ExtraInput,
    UnknownFlag(String),
}

struct Args {
    bin_name: String,
    input_file: Option<String>,
    preprocess_only: bool,
    map_binary_at: usize,
    show_help: bool,
}

impl Args {
    fn validate(&self) -> bool {
        if self.show_help {
            return false;
        }
        self.input_file.is_some()
    }

    fn usage(&self) -> String {
        format!(
            "usage: {} [OPTIONS] <input file>
options:
    -h, --help\tShow this message.
    -p, --preprocess-only\tStop after running the preprocessor and print the instructions.
    -x, --program-offset\tAddress to load program at initialzed pc.
",
            self.bin_name
        )
    }
}

impl Default for Args {
    fn default() -> Self {
        Self {
            bin_name: ".".to_string(),
            input_file: None,
            preprocess_only: false,
            show_help: false,
            map_binary_at: 0x0,
        }
    }
}

fn parse_args(args: &[String]) -> Result<Args, ArgsError> {
    let mut out = Args {
        bin_name: args[0].to_string(),
        ..Args::default()
    };
    for arg in &args[1..] {
        if let Some(flag) = arg.strip_prefix("--") {
            match flag {
                "preprocess-only" => out.preprocess_only = true,
                "help" => out.show_help = true,
                x => return Err(ArgsError::UnknownFlag(x.to_string())),
            }
        } else if let Some(flag) = arg.strip_prefix("-") {
            match flag {
                "p" => out.preprocess_only = true,
                "h" => out.show_help = true,
                x => return Err(ArgsError::UnknownFlag(x.to_string())),
            }
        } else {
            if out.input_file.is_some() {
                return Err(ArgsError::ExtraInput);
            }
            out.input_file = Some(arg.to_string());
        }
    }
    Ok(out)
}

fn main() -> Result<(), String> {
    let args_vec: Vec<_> = env::args().collect();
    let args = parse_args(&args_vec).map_err(|x| x.to_string())?;
    if !args.validate() {
        println!("{}", args.usage());
        return Ok(());
    }

    let input_file_path = args.input_file.clone().unwrap();
    let file =
        File::open(Path::new(&input_file_path)).map_err(|x| format!("failed to open: {}", x))?;
    let mut output: Vec<u8> = Vec::new();

    let mut processor = PreProcessor::default();
    pp_macros::setup_std_macros(&mut processor);
    let mut reader = BufReader::new(file);
    let mut content = String::new();
    reader
        .read_to_string(&mut content)
        .map_err(|_| "failed to read file".to_string())?;
    processor
        .handle(&content)
        .map_err(|e| format!("failed to resolve: {}", e))?;
    if args.preprocess_only {
        todo!("wip rebuilding");
    } else {
        let bin: BinaryFile = processor
            .try_into()
            .map_err(|e| format!("build binary: {}", e))?;
        bin.to_bytes(&mut output);
    }
    let mut stdout = io::stdout().lock();
    stdout.write_all(&output).map_err(|e| format!("{}", e))?;
    let output_path = Path::new(&input_file_path).with_extension("bin");
    let mut output_file =
        File::create(&output_path).map_err(|e| format!("failed to create output file: {}", e))?;
    output_file
        .write_all(&output)
        .map_err(|e| format!("failed to write to output file: {}", e))?;
    println!("Binary file written to: {}", output_path.display());
    Ok(())
}
