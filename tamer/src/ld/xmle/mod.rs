// xmle object files
//
//  Copyright (C) 2014-2021 Ryan Specialty Group, LLC.
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

//! `xmle` file construction and processing.
//!
//! This file format exists for compatibility with the old compiler
//!   written in XSLT; it will be removed in the future.
//!
//!
//! `xmle` File Format
//! ==================
//! The `xmle` file contains:
//!
//!   - `l:dep` representing the topologically sorted symbols associated
//!        with the linked objects,
//!          along with metadata;
//!   - `l:map-from`, a list of source field identifiers;
//!   - `l:map-exec`, the linked input map text;
//!   - `l:retmap-exec`, the linked return map text;
//!   - `l:static`, the linked static section that may be initialized once
//!       as soon as the program is loaded into memory; and
//!   - `l:exec`, the linked main program text.
//!
//! ```xml
//! <package xmlns="http://www.lovullo.com/rater"
//!          xmlns:preproc="http://www.lovullo.com/rater/preproc"
//!          xmlns:l="http://www.lovullo.com/rater/linker"
//!          title="suppliers/tax"
//!          program="true"
//!          name="suppliers/tax"
//!          __rootpath="../">
//!   <l:dep>
//!     <preproc:sym type="func"
//!                  dim="0"
//!                  dtype="float"
//!                  name="min"
//!                  src="../rater/core/numeric/minmax"
//!                  desc="Return the lesser value"/>
//!   </l:dep>
//!   <l:map-from>
//!     <l:from name="latest_operation_hour"/>
//!   </l:map-from>
//!   <l:map-exec>
//!     function(input,callback) {)
//!   </l:map-exec>
//!   <l:retmap-exec>
//!     function(input,callback) {)
//!   </l:retmap-exec>
//!   <l:static>
//!     function func_min(args,min1,min2) {return min1;}
//!   </l:static>
//!   <l:exec>C['CMP_OP_EQ'] = 1;</l:exec>
//! </package>
//! ```

mod section;
pub mod xir;

pub use section::{Section, SectionIter, Sections, SectionsIter};
