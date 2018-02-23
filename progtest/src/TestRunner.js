/**
 * Test case runner
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

const { Class } = require( 'easejs' );


/**
 * Run test cases and report results
 */
module.exports = Class( 'TestRunner',
{
    /**
     * SUT
     *
     * @type {Program}
     */
    'private _program': null,

    /**
     * Test reporter
     *
     * @type {TestReporter}
     */
    'private _reporter': null,


    /**
     * Initialize runner for program PROGRAM
     *
     * @param {TestReporter} reporter test reporter
     * @param {Program}      program  SUT
     */
    constructor( reporter, program )
    {
        // primitive check to guess whether this might be a program
        if ( typeof program.rater !== 'function' )
        {
            throw TypeError( "program#rater is not a function" );
        }

        this._reporter = reporter;
        this._program  = program;
    },


    /**
     * Run set of test cases
     *
     * @param {Array<TestCase>} dfns array of TestCases
     *
     * @return {Promise} promise to complete test cases, yielding results
     */
    'public runTests'( dfns )
    {
        const total = dfns.length;

        this._reporter.preRun( total );

        return this._runAsync( dfns ).then(
            results => ( this._reporter.done( results ), results )
        );
    },


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
    'private _runAsync'( dfns )
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
                const result = this._runTest( dfn, results.length, total );

                results.push( result );

                setTimeout( runNext, 0 );
            };

            runNext();
        } );
    },


    /**
     * Run individual test case
     *
     * @param {Object<TestCase>} _      source test case
     * @param {number}           test_i test index
     * @param {number}           total  total number of tests
     *
     * @return {Object<desc,i,total,failures>} test results
     */
    'private _runTest'( { description: desc, data, expect }, test_i, total )
    {
        // no input map---#rate uses params directly
        const result = this._program.rater( data ).vars;

        const cmp = Object.keys( expect ).map(
            field => [
                field,
                this._deepCompare( expect[ field ], result[ field ] )
            ]
        );

        const failures = cmp.filter( ( [ , ok ] ) => !ok )
              .map( ( [ field ] ) => ( {
                  field:  field,
                  expect: expect[ field ],
                  result: result[ field ],
              } ) );

        const succeeded = cmp.length - failures.length;

        const result_data = {
            desc:     desc,
            i:        test_i,
            total:    cmp.length,
            failures: failures,
            given:    data,
            expect:   expect,
        };

        this._reporter.testCaseResult( result_data, total );

        return result_data;
    },


    /**
     * Recursively compare values (scalar, array)
     *
     * @param {number|Array<number>} x first value
     * @param {number|Array<number>} y second value
     *
     * @return {boolean} whether X deeply equals Y
     */
    'private _deepCompare'( x, y )
    {
        // vector/matrix/etc
        if ( Array.isArray( x ) )
        {
            return Array.isArray( y )
                && ( x.length === y.length )
                && x.every( ( xval, i ) => xval === y[ i ] );
        }

        // scalar
        return x === y;
    },
} );
