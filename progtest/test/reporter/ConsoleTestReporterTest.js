/**
 * Tests ConsoleTestReporter
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

const { expect } = require( 'chai' );
const Sut = require( '../../src/reporter/ConsoleTestReporter' );


describe( "ConsoleTestReporter", () =>
{
    describe( "#testCaseResult", () =>
    {
        it( "outputs indicator for each test case", () =>
        {
            let output = '';

            const stdout = { write: str => output += str };
            const sut    = Sut( stdout );

            [
                { i: 0, failures: [] },
                { i: 1, failures: [] },
                { i: 2, failures: [ {} ] },
                { i: 3, failures: [ {}, {} ] },
                { i: 4, failures: [] },
            ].forEach(
                result => sut.testCaseResult( result, 5 )
            );

            expect( output ).to.equal( '..FF.' );
        } );


        it( "outputs line break with count after 40 cases", () =>
        {
            let output = '';

            const stdout = { write: str => output += str };
            const sut    = Sut( stdout );

            const results = ( new Array( 130 ) ).join( '.' ).split( '.' )
                .map( ( _, i ) => ( { i: i, failures: [] } ) );

            results.forEach(
                result => sut.testCaseResult( result, 130 )
            );

            expect( output ).to.equal(
                ( new Array( 51 ) ).join( '.' ) + '  50/130\n' +
                ( new Array( 51 ) ).join( '.' ) + '  100/130\n' +
                ( new Array( 31 ) ).join( '.' )
            );
        } );
    } );


    describe( "done", () =>
    {
        it( "outputs report of failures to stdout", () =>
        {
            let output = '';

            const stdout = { write: str => output += str };

            const results = [
                { i: 0, total: 1, desc: "test 0", failures: [] },
                { i: 1, total: 2, desc: "test 1", failures: [] },

                {
                    i:        2,
                    total:    3,
                    desc:     "test 2",
                    failures: [
                        {
                            field:  "foo",
                            expect: [ 1 ],
                            result: [ 2, 3 ]
                        },
                    ],
                },
                {
                    i:        3,
                    total:    4,
                    desc:     "test 3",
                    failures: [
                        {
                            field:  "bar",
                            expect: 2,
                            result: 3,
                        },
                        {
                            field:  "baz",
                            expect: [ [ 4 ] ],
                            result: [ 5 ],
                        }
                    ],
                },
            ];

            const stringified = results.map(
                result => result.failures.map(
                    failure => ( {
                        expect: JSON.stringify( failure.expect ),
                        result: JSON.stringify( failure.result ),
                    } )
                )
            );

            Sut( stdout ).done( results );

            const fail_output = output.match( /\n\n\[#3\](.|\n)*\n\n/ )[0];

            // 1-indexed output
            expect( fail_output ).to.equal(
                `\n\n` +
                `[#3] test 2\n` +
                `  foo:\n` +
                `    expected: ` + stringified[ 2 ][ 0 ].expect + `\n` +
                `    result:   ` + stringified[ 2 ][ 0 ].result + `\n` +
                `\n` +
                `[#4] test 3\n` +
                `  bar:\n` +
                `    expected: ` + stringified[ 3 ][ 0 ].expect + `\n` +
                `    result:   ` + stringified[ 3 ][ 0 ].result + `\n` +
                `  baz:\n` +
                `    expected: ` + stringified[ 3 ][ 1 ].expect + `\n` +
                `    result:   ` + stringified[ 3 ][ 1 ].result + `\n\n`
            );
        } );


        it( "outputs summary on last line of stdout", () =>
        {
            let output = '';

            const stdout = { write: str => output += str };
            const sut    = Sut( stdout, {} );

            Sut( stdout ).done( [
                { i: 0, total: 1, failures: [] },
                { i: 1, total: 2, failures: [] },
                { i: 2, total: 3, failures: [ {} ] },
                { i: 3, total: 4, failures: [ {}, {} ] },
                { i: 4, total: 5, failures: [] },
            ] );

            const lines = output.split( '\n' );

            // preceded by empty line
            expect( lines[ lines.length - 2 ] ).to.equal( "" );

            // last line
            expect( lines[ lines.length - 1 ] ).to.equal(
                `5 tests, 2 failed (15 assertions, 3 failures)`
            );
        } );
    } );
} );
