/**
 * Date resolver for test case reader
 *
 *  Copyright (C) 2014-2019 Ryan Specialty Group, LLC.
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


const { Trait }  = require( 'easejs' );
const TestReader = require( './TestReader' );


/**
 * Resolve dates of the format MM/DD/YYYY to Unix timestamps
 *
 * This allows easily readable dates to be included in test cases without
 * having to worry about Unix timestamps.  For higher precision, Unix
 * timestamps must be used.
 */
module.exports = Trait( 'DateResolver' )
    .implement( TestReader )
    .extend(
{
    /**
     * Load test cases and resolve dates
     *
     * Dates will be replaced with Unix timestamps.
     *
     * @param {*} src data source
     *
     * @return {Array<TestCase>} array of test cases
     */
    'abstract override public loadCases'( src )
    {
        const data = this.__super( src );

        return data.map(
            testcase => testcase.mapEachValue(
                value => ( /^\d{2}\/\d{2}\/\d{4}$/.test( value ) )
                    ? ( ( new Date( value ) ).getTime() / 1000 )
                    : value
            )
        );
    }
} );
