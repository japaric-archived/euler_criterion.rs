//! Collect information about the underlying system

use std::fs::File;
use std::io::{Write, self};
use std::process::Command;

use context::Context;

// TODO Try other sources
/// Get information about the system CPU
pub fn cpu(ctxt: &Context) -> io::Result<()> {
    const CPU_FILE: &'static str = "cpu";

    let output = try!(Command::new("lscpu").output());

    if output.status.success() {
        let mut file = try!(File::create(ctxt.output_dir().join(CPU_FILE)));
        try!(file.write_all(&output.stdout));
    }

    Ok(())
}
