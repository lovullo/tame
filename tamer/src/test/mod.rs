// Mocks, stubs, and other stuff for testing
//
//  Copyright (C) 2014-2022 Ryan Specialty Group, LLC.
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

pub mod quick_xml;

/// Ensures that tests will hit debug assertions.
///
/// Debug assertions are used to enforce invariants that would certainly be
///   hit by tests if violated,
///     and so have no need to be included in release builds.
///
/// If this test fails,
///   then optimization settings are inhibiting debug assertions.
/// See the documentation for [`debug_assert!`] for more information.
#[test]
#[should_panic]
fn uses_debug_assertions() {
    debug_assert!(false, "should panic");
}
