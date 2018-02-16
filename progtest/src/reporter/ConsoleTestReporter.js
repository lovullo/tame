/**
 * Console test reporter
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
 * Real-time reporting of test cases to the console
 *
 * Test cases will be output in a block of dots (success) or 'F's (failure),
 * in a style similar to PHPUnit.  If failures occur, they will be output to
 * in more detail after all tests have run.
 */
module.exports = Class( 'ConsoleTestReporter',
{
    /**
     * Standard out
     *
     * @type {Object} standard out
     */
    'private _stdout': null,


    /**
     * Initialize reporter with target console
     *
     * STDOUT must follow Node.js' API.
     *
     * @param {Object} stdout standard out
     */
    constructor( stdout )
    {
        this._stdout = stdout;
    },


    /**
     * Invoked before tests are run
     *
     * The only information provided here is the number of test cases to be
     * run, which can be used to produce a progress indicator.
     *
     * @param {number} total number of test cases
     *
     * @return {undefined}
     */
    'public preRun'( total )
    {
        // this reporter does nothing with this method
    },


    /**
     * Invoked for each test case immediately after it has been run
     *
     * For the format of RESULT, see TestRunner.
     *
     * @param {Object} result test case result
     *
     * @return {undefined}
     */
    'public testCaseResult'( result, total )
    {
        const { i, failures } = result;

        const ind = ( failures.length === 0 )
            ? '.'
            : 'F';

        const sep = ( i % 50 === 49 )
            ? `  ${i+1}/${total}\n`
            : '';

        this._stdout.write( ind + sep );
    },


    /**
     * Invoked after all test cases have been run
     *
     * RESULTS is an array containing each result that was previously
     * reported to `#testCaseResult`.
     *
     * A final line will be output, preceded by an empty line, summarizing
     * the number of tests, assertions, and failures for each.
     *
     * @param {Array<Object>} results all test results
     *
     * @return {undefined}
     */
    'public done'( results )
    {
        this._outputFailureReport( results );
        this._outputSummary( results );
    },


    /**
     * For each failure, output each expected and resulting value
     *
     * Failures are prefixed with a 1-indexed number.
     *
     * @param {Object} result test case result}
     *
     * @return {undefined}
     */
    'private _outputFailureReport'( results )
    {
        const report = results
            .filter( ( { failures } ) => failures.length > 0 )
            .map( this._reportTestFailure.bind( this ) )
            .join( '\n' )

        this._stdout.write( "\n\n" + report );
    },


    /**
     * Generate report for test case failure
     *
     * @param {Object<i,desc,failures>} _ test case result data
     *
     * @return {string} report
     */
    'private _reportTestFailure'( { i, desc, failures } )
    {
        return `[#${i+1}] ${desc}\n` +
            failures.map( ( { field, expect, result } ) =>
                `  ${field}:\n` +
                `    expected: ` + JSON.stringify( expect ) + `\n` +
                `    result:   ` + JSON.stringify( result ) + `\n`
            ).join( '' );
    },


    /**
     * Output a line, preceded by an empty line, summarizing the number of
     * tests, assertions, and failures for each
     *
     * @param {Array<Object>} results all test results
     *
     * @return {undefined}
     */
    'private _outputSummary'( results )
    {
        const [ failed, afailed, acount ] = results.reduce(
            ( [ failed, afailed, acount ], { failures, total } ) =>
                [
                    ( failed + +( failures.length > 0 ) ),
                    ( afailed + failures.length ),
                    ( acount + total )
                ],
            [ 0, 0, 0 ]
        );

        const test_total = results.length;

        this._stdout.write(
            `\n${test_total} tests, ${failed} failed (` +
            `${acount} assertions, ${afailed} failures)`
        );
    },
} );
