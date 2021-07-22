/**
 * Test case
 *
 *  Copyright (C) 2014-2021 Ryan Specialty Group, LLC.
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

const { Class }  = require( 'easejs' );


module.exports = Class( 'TestCase',
{
    /**
     * Test case data
     *
     * @type {Object} test case data
     */
    'private _caseData': {},

    get description()
    {
        return this._caseData.description || "";
    },

    get allow_failures()
    {
        return this._caseData.allow_failures || false;
    },

    get data()
    {
        return this._caseData.data || {};
    },

    get expect()
    {
        return this._caseData.expect || {};
    },


    constructor( case_data )
    {
        this._caseData = case_data;
    },


    'public mapEachValue'( callback )
    {
        const [ new_data, new_expect ] = [ this.data, this.expect ].map( src =>
        {
            const new_src = {};

            Object.keys( src ).forEach(
                key => new_src[ key ] = this._visitDeep(
                    src[ key ],
                    callback
                )
            );

            return new_src;
        } );

        return module.exports( {
            description:    this.description,
            allow_failures: this.allow_failures,
            data:           new_data,
            expect:         new_expect,
        } );
    },


    /**
     * Recursively resolve constants
     *
     * Only scalars and arrays are supported
     *
     * @param {number|Array} input  scalar or array of inputs
     * @param {Object}       consts constant mapping
     *
     * @return {number|Array} resolved value(s)
     */
    'private _visitDeep'( val, callback )
    {
        if ( Array.isArray( val ) )
        {
            return val.map(
                x => this._visitDeep( x, callback )
            );
        }

        return callback( val );
    }
} );
