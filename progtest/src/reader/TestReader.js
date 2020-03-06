/**
 * Test case reader
 *
 *  Copyright (C) 2014-2020 Ryan Specialty Group, LLC.
 *
 *  This file is part of TAME.
 *
 *  TAME is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

"use strict";

const { Interface } = require( 'easejs' );


module.exports = Interface( 'TestReader',
{
    /**
     * Load test cases from an implementation-defined data source SRC
     *
     * The produced object will be an array of cases, each containing a
     * `description`, `data`, and `expect`.
     *
     * @param {*} src data source
     *
     * @return {Array<TestCase>} array of test cases
     */
    'public loadCases': [ 'src' ],
} );
