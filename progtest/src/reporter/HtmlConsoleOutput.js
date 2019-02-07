/**
 * Hyperlinks for console test reporter
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

const { Trait }           = require( 'easejs' );
const ConsoleTestReporter = require( './ConsoleTestReporter' );


/**
 * Format console output as interactive HTML.
 *
 * Spacing and line breaks will be preserved.  Test cases will be made
 * clickable (both the indicators and failure output); it is up to the
 * caller to hook the links to do something useful.
 */
module.exports = Trait( 'HtmlConsoleOutput' )
    .extend( ConsoleTestReporter,
{
    /**
     * Produce string for test case result
     *
     * The result is the concatenation of result and progress indicators.
     *
     * @param {Object} result test case result
     * @param {number} total  total number of test cases
     *
     * @return {string} output string
     */
    'override virtual public createTestCaseResult'( result, total )
    {
        return this.__super( result, total ).replace( /\n/g, "<br />" );
    },


    /**
     * Produce test case result indicator
     *
     * The indicator is a single `.` on success, and `F` on failure.
     *
     * @param {Object} result test case result
     * @param {number} total  total number of test cases
     *
     * @return {string} output string
     */
    'override protected getInd'( result, total )
    {
        const { i, failures, desc } = result;

        const ind = this.__super( result );
        const title = `[#${i}] ` + this._titleify( desc );

        return `<a href="#" data-case-index="${i}" title="${title}">${ind}</a>`;
    },


    /**
     * Format string for use as a truncated anchor title
     *
     * The title will be truncated to 60 characters (including the ellipsis,
     * if one is added), and will escape double quotes and closing angled
     * brackets.
     *
     * @param {string} str source string
     *
     * @return {string} formatted string
     */
    'private _titleify'( str )
    {
        const newstr = ( str.length < 60 )
            ? str
            : str.substr( 0, 57 ) + '...';

        return newstr
            .replace( />/g, "&gt;" )
            .replace( /"/g, "&dquo;" );
    },


    /**
     * Produce progress indicator for test case result
     *
     * Progress will be reported numerically every 50 test cases, followed
     * by a newline.
     *
     * @param {Object} result test case result
     * @param {number} total  total number of test cases
     *
     * @return {string} progress string
     */
    'override protected createResultProgress'( results, total )
    {
        return this.__super( results, total ).replace( / /g, '&nbsp;' );
    },


    /**
     * Create diff output for failed assertions
     *
     * @param {Object} result failure data
     *
     * @return {string} diff
     */
    'override protected createFailureDiff'( failure )
    {
        return this._htmlizeText( this.__super( failure ) );
    },


    /**
     * Create heading for individual test case failure
     *
     * @param {number} i    test case index
     * @param {string} desc test case description
     *
     * @return {string} heading
     */
    'override protected createFailureHeading'( i, desc )
    {
        return `<a href="#" data-case-index="${i}">` +
            `[#${i+1}]</a> ${desc}<br />`;
    },


    /**
     * Compile failure result strings into a single report
     *
     * @param {Array<string>} results failures
     *
     * @return {string} combined report
     */
    'override protected combineFailureResults'( results )
    {
        return "<br /><br />" + results.join( "<br />" );
    },


    /**
     * Output a line, preceded by an empty line, summarizing the number of
     * tests, assertions, and failures for each
     *
     * @param {number} test_total total number of test cases run
     * @param {number} failed     number of failed test cases
     * @param {number} acount     total number of assertions
     * @param {number} failed     number of assertion failures
     *
     * @return {string} summary line
     */
    'override protected createSummaryLine'( test_total, failed, acount, afailed )
    {
        return this._htmlizeText(
            this.__super( test_total, failed, acount, afailed )
        );
    },


    /**
     * Format string for output in HTML while maintaining appearance
     *
     * Spaces are converted into non-breaking spaces (so as not to be
     * normalized) and line breaks are converted into `br` tags.
     *
     * @param {string} source string
     *
     * @return {string} formatted string
     */
    'private _htmlizeText'( str )
    {
        return str
            .replace( / /g, "&nbsp;" )
            .replace( /\n/g, '<br />' );
    },
} );
