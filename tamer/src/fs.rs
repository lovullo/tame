// Light filesystem abstractions
//
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

//! Lightweight filesystem abstraction.
//!
//! This abstraction is intended to provide generics missing from Rust core,
//!   but makes no attempt to be comprehensive---it
//!     includes only what is needed for TAMER.
//!
//!   - [`File`] provides a trait for operating on files; and
//!   - [`Filesystem`] provides a generic way to access files by path.
//!
//! This implements traits directly atop of Rust's core structs where
//!   possible.
//!
//!
//! Visiting Files Once
//! ===================
//! [`VisitOnceFilesystem`] produces [`VisitOnceFile::FirstVisit`] the first
//!   time it encounters a given path,
//!     and [`VisitOnceFile::Visited`] every time thereafter.

use std::collections::hash_map::RandomState;
use std::collections::HashSet;
use std::ffi::OsString;
use std::fs;
use std::hash::BuildHasher;
use std::io::Result;
use std::path::Path;

/// A file.
pub trait File
where
    Self: Sized,
{
    fn open<P: AsRef<Path>>(path: P) -> Result<Self>;
}

impl File for fs::File {
    fn open<P: AsRef<Path>>(path: P) -> Result<Self> {
        Self::open(path)
    }
}

/// A filesystem.
///
/// Opening a file (using [`open`](Filesystem::open)) proxies to `F::open`.
/// The type of files opened by this abstraction can therefore be controlled
///   via generics.
pub trait Filesystem<F: File>
where
    Self: Sized,
{
    fn open<P: AsRef<Path>>(&mut self, path: P) -> Result<F> {
        F::open(path)
    }
}

/// A potentially visited [`File`].
///
/// See [`VisitOnceFilesystem`] for more information.
#[derive(Debug, PartialEq)]
pub enum VisitOnceFile<F: File> {
    /// First time visiting file at requested path.
    FirstVisit(F),

    /// Requested path has already been visited.
    Visited,
}

impl<F: File> File for VisitOnceFile<F> {
    fn open<P: AsRef<Path>>(path: P) -> Result<Self> {
        F::open(path).map(|file| Self::FirstVisit(file))
    }
}

/// Opens each path only once.
///
/// When a [`File`] is first opened,
///   it will be wrapped in [`VisitOnceFile::FirstVisit`]
/// Subsequent calls to `open` will yield
///   [`VisitOnceFile::Visited`] without attempting to open the file.
///
/// A file will not be marked as visited if it fails to be opened.
pub struct VisitOnceFilesystem<S = RandomState>
where
    S: BuildHasher,
{
    visited: HashSet<OsString, S>,
}

impl<S> VisitOnceFilesystem<S>
where
    S: BuildHasher + Default,
{
    /// New filesystem with no recorded paths.
    pub fn new() -> Self {
        Self {
            visited: Default::default(),
        }
    }

    /// Number of visited paths.
    pub fn visit_len(&self) -> usize {
        self.visited.len()
    }
}

impl<S, F> Filesystem<VisitOnceFile<F>> for VisitOnceFilesystem<S>
where
    S: BuildHasher,
    F: File,
{
    /// Open a file, marking `path` as visited.
    ///
    /// The next time the same path is requested,
    ///   [`VisitOnceFile::Visited`] will be returned.
    ///
    /// `path` will not be marked as visited if opening fails.
    fn open<P: AsRef<Path>>(&mut self, path: P) -> Result<VisitOnceFile<F>> {
        let ostr = path.as_ref().as_os_str();

        if self.visited.contains(ostr) {
            return Ok(VisitOnceFile::Visited);
        }

        VisitOnceFile::open(ostr).and_then(|file| {
            self.visited.insert(ostr.to_os_string());
            Ok(file)
        })
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::path::PathBuf;

    #[derive(Debug, PartialEq)]
    struct DummyFile(PathBuf);

    impl File for DummyFile {
        fn open<P: AsRef<Path>>(path: P) -> Result<Self> {
            Ok(Self(path.as_ref().to_path_buf()))
        }
    }

    #[test]
    fn vist_once() {
        let mut fs = VisitOnceFilesystem::<RandomState>::new();
        let path: PathBuf = "foo/bar".into();
        let result = fs.open(path.clone()).unwrap();

        // First time, return file.
        assert_eq!(VisitOnceFile::FirstVisit(DummyFile(path.clone())), result);

        // Second time, already visited.
        let result_2: VisitOnceFile<DummyFile> = fs.open(path).unwrap();
        assert_eq!(VisitOnceFile::Visited, result_2);
    }
}
