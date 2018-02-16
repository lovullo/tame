/**
 * Tests TestReader
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

const { expect }       = require( 'chai' );
const { Class }        = require( 'easejs' );
const Sut              = require( '../src/TestRunner' );
const TestReporter     = require( '../src/reporter/TestReporter' );
const NullTestReporter = require( '../src/reporter/NullTestReporter' );


describe( "TestRunner", () =>
{
    it( "runs each test against given program", () =>
    {
        const given = [];

        const program = {
            rater( data )
            {
                return rate_results[ given.push( data ) - 1 ];
            }
        };

        const test_cases = [
            {
                description: "first",
                data:        { a: 1 },
                expect:      { foo: 1 },
            },
            {
                description: "second",
                data:        { a: 2 },
                expect:      {
                    foo: [ 1, 2 ],
                    bar: [ 3, 1 ],
                    baz: [ 4, 2 ],
                },
            },
        ];

        const rate_results = [
            // no failures
            { vars: { foo: 1 } },

            // bar, baz failures
            { vars: {
                foo: [ 1, 2 ],
                bar: [ 3, 4 ],
                baz: [ 4, 5 ],
            } },
        ];

        const expect_failures = [
            [],
            [
                {
                    field: 'bar',
                    expect: test_cases[ 1 ].expect.bar,
                    result: rate_results[ 1 ].vars.bar,
                },
                {
                    field: 'baz',
                    expect: test_cases[ 1 ].expect.baz,
                    result: rate_results[ 1 ].vars.baz,
                },
            ]
        ];

        const results = Sut( NullTestReporter(), program )
            .runTests( test_cases );

        test_cases.forEach( ( test_case, i ) =>
        {
            const result = results[ i ];

            expect( result.desc ).to.equal( test_case.description );
            expect( result.i ).to.equal( i );
            expect( result.total ).to.equal(
                Object.keys( test_case.expect ).length
            );
            expect( result.failures ).to.deep.equal( expect_failures[ i ] );
        } );
    } );


    it( "invokes reporter before, during, and after test cases", done =>
    {
        let pre     = false;
        let results = [];

        const program = { rater: () => ( { vars: {} } ) };

        const mock_reporter = Class.implement( TestReporter ).extend(
        {
            preRun( total )
            {
                expect( total ).to.equal( 2 );
                pre = true;
            },

            testCaseResult( result, total )
            {
                expect( pre ).to.equal( true );
                expect( total ).to.equal( 2 );

                results.push( result );
            },

            done( given_results )
            {
                expect( pre ).to.equal( true );
                expect( results ).to.deep.equal( given_results );

                done();
            },
        } )();

        // see done() above
        Sut( mock_reporter, program ).runTests( [
            { description: '', data: {}, expect: {} },
            { description: '', data: {}, expect: {} },
        ] );
    } );
} );
