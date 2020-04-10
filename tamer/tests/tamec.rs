//  Copyright (C) 2014-2020 Ryan Specialty Group, LLC.
//
//  This file is part of TAME.
//
//  This program is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.

use assert_cmd::prelude::*;
use predicates::prelude::*;
use std::process::Command;

#[test]
fn compile_invalid_argument() -> Result<(), Box<dyn std::error::Error>> {
    let mut cmd = Command::cargo_bin("tamec")?;
    cmd.arg("-q");
    cmd.assert()
        .failure()
        .code(exitcode::USAGE)
        .stderr(predicate::str::contains("Unrecognized option:"));

    Ok(())
}

#[test]
fn compile_missing_input_file() -> Result<(), Box<dyn std::error::Error>> {
    let mut cmd = Command::cargo_bin("tamec")?;
    cmd.arg("--emit").arg("xmlo");
    cmd.assert()
        .failure()
        .code(exitcode::USAGE)
        .stderr(predicate::str::contains("INPUT"));

    Ok(())
}

#[test]
fn compile_missing_emit() -> Result<(), Box<dyn std::error::Error>> {
    let mut cmd = Command::cargo_bin("tamec")?;
    cmd.arg("foobar");
    cmd.assert()
        .failure()
        .code(exitcode::USAGE)
        .stderr(predicate::str::contains("--emit xmlo"));

    Ok(())
}

#[test]
fn compile_invalid_emit() -> Result<(), Box<dyn std::error::Error>> {
    let mut cmd = Command::cargo_bin("tamec")?;
    cmd.arg("foobar");
    cmd.arg("--emit").arg("foo");
    cmd.assert()
        .failure()
        .code(exitcode::USAGE)
        .stderr(predicate::str::contains("--emit xmlo"));

    Ok(())
}

#[test]
fn compile_input_file_does_not_exist() -> Result<(), Box<dyn std::error::Error>>
{
    let mut cmd = Command::cargo_bin("tamec")?;
    cmd.arg("foobar.xml");
    cmd.arg("--emit").arg("xmlo");
    cmd.assert()
        .failure()
        .code(1)
        .stderr(predicate::str::contains("No such file or directory"));

    Ok(())
}

#[test]
fn compile_input_file_wrong_extension() -> Result<(), Box<dyn std::error::Error>>
{
    let mut cmd = Command::cargo_bin("tamec")?;
    cmd.arg("foobar.php");
    cmd.arg("--emit").arg("xmlo");
    cmd.assert().failure().stderr(predicate::str::contains(
        "foobar.php: file format not recognized",
    ));

    Ok(())
}
