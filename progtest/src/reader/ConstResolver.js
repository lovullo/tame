/**
 * Constant resolver for test case reader
 *
 *  Copyright (C) 2018 R-T Specialty, LLC.
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
 * Resolve program constants by replacing them with their numeric value
 *
 * The result is a loaded YAML file that contains only numeric input.  All
 * non-numeric input is interpreted as a constant.
 *
 * Any non-numeric value is considered to be a constant, so it is important
 * to perform all other data transformations before applying constant
 * resolution.
 */
module.exports = Trait( 'ConstResolver' )
    .implement( TestReader )
    .extend(
{
    /**
     * Program from which to load constants
     *
     * @type {Program}
     */
    'private _program': null,


    /**
     * Initialize with program from which to load constants
     *
     * @param {Program} program source program
     */
    __mixin( program )
    {
        this._program = program;
    },


    /**
     * Load test cases and resolve constants
     *
     * @param {*} src data source
     *
     * @return {Array<TestCase>} array of test cases
     */
    'abstract override public loadCases'( yaml )
    {
        const data       = this.__super( yaml );
        const { consts } = this._program.rater;

        return data.map(
            testcase => testcase.mapEachValue(
                value => this._resolve( value, consts )
            )
        );
    },


    /**
     * Resolve constant
     *
     * If the constant is not known (via CONSTS), an error is thrown.
     *
     * @param {number|Array} input  scalar or array of inputs
     * @param {Object}       consts constant mapping
     *
     * @throws {Error} if constant is unknown in CONSTS
     *
     * @return {number|Array} resolved value(s)
     */
    'private _resolve'( input, consts )
    {
        // already a number, return as-is
        if ( !isNaN( +input ) )
        {
            return input;
        }

        const result = consts[ input ];
        if ( result === undefined )
        {
            throw Error( `unknown constant: ${input}` );
        }

        return result;
    }
} );
