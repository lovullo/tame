/**
 * Test reporter
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


/**
 * Real-time reporting of test cases
 */
module.exports = Interface( 'TestReporter',
{
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
    'public preRun': [ 'total' ],


    /**
     * Invoked for each test case immediately after it has been run
     *
     * For the format of RESULT, see TestRunner.
     *
     * @param {Object} result test case result
     *
     * @return {undefined}
     */
    'public testCaseResult': [ 'result' ],


    /**
     * Invoked after all test cases have been run
     *
     * RESULTS is an array containing each result that was previously
     * reported to `#testCaseResult`.
     *
     * @param {Array<Object>} results all test results
     *
     * @return {undefined}
     */
    'public done': [ 'results' ],
} );
