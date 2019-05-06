/**
 * Async test case runner
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

const { Class }  = require( 'easejs' );
const TestRunner = require( './TestRunner' );


/**
 * Run test cases asynchronously and report results
 *
 * This allows the browser to repaint between cases.
 */
module.exports = Class( 'AsyncTestRunner' )
    .extend( TestRunner,
{
    /**
     * Run all tests asynchronously
     *
     * TODO: This significantly slows down the runner!  The better option
     * would be to go back to sync and put it in a Web Worker in the client,
     * which would also async updating of the UI.
     *
     * @param {Array<TestCase>} dfns test case definitions
     *
     * @return {Promise} promise to complete test cases, yielding results
     */
    'override protected runAllTests'( dfns )
    {
        const total = dfns.length;

        return new Promise( ( resolve, reject ) =>
        {
            const results = [];

            const runNext = () =>
            {
                if ( dfns.length === 0 )
                {
                    resolve( results );
                    return;
                }

                const dfn    = dfns.shift();
                const result = this.runTest( dfn, ( results.length + 1 ), total );

                results.push( result );

                setTimeout( runNext, 0 );
            };

            runNext();
        } );
    },
} );
