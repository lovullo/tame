/**
 * Test case runner
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

        return this.runAllTests( dfns ).then(
            results => ( this._reporter.done( results ), results )
        );
    },


    /**
     * Run all tests
     *
     * This may be overridden by subtypes to change how the tests are run
     * (for example, to run each asynchronously).
     *
     * @param {Array<TestCase>} dfns test case definitions
     *
     * @return {Promise} promise to complete test cases, yielding results
     */
    'virtual protected runAllTests'( dfns )
    {
        const total = dfns.length;

        return Promise.resolve(
            dfns.map( ( dfn, i ) => this.runTest( dfn, ( i + 1 ), total ) )
        );
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
    'protected runTest'( { description: desc, data, expect }, test_i, total )
    {
        // no input map---#rate uses params directly
        const result = this._tryRun( data );

        const cmp = Object.keys( expect ).map(
            field => [
                field,
                this._deepCompare( expect[ field ], result[ field ] )
            ]
        );

        const failures = ( result instanceof Error )
            ? [ {
                field:  "error",
                expect: "",
                result: result.message,
            } ]
            : cmp.filter( ( [ , ok ] ) => !ok )
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
     * Attempt test case, returning error on failure
     *
     * If an error is thrown (e.g. terminating classification or unknown
     * input), then it will be returned in place of the results.
     *
     * @param {Object} data input data
     *
     * @return {Object|Error} result or error
     */
    'private _tryRun'( data )
    {
        // no input map---#rate uses params directly
        try
        {
            this._verifyKnownParams( data );

            return this._program.rater( data ).vars;
        }
        catch( e )
        {
            return e;
        }
    },


    /**
     * Verify that all provided inputs match known params
     *
     * If a given input is not known for the rater for the current program,
     * an Error will be thrown with a comma-delimited list of all unknown
     * params.
     *
     * @param {Object} data input data
     *
     * @return {undefined}
     *
     * @throws Error when unknown input is found
     */
    'private _verifyKnownParams'( data )
    {
        const params = this._program.rater.params || {};

        const unknown = Object.keys( data )
            .filter( param => params[ param ] === undefined );

        if ( unknown.length > 0 )
        {
            throw Error( "Unknown params: " + unknown.join( ", " ) );
        }
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
                && x.every( ( xval, i ) => this._deepCompare( xval, y[ i ] ) );
        }

        // scalar
        return x === y;
    },
} );
