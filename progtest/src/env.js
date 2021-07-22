/**
 * Environment-specific runner initialization
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

const yaml_reader = require( 'js-yaml' );

const {
    TestCase,
    TestRunner,
    AsyncTestRunner,

    reader: {
        ConstResolver,
        DateResolver,
        YamlTestReader
    },

    reporter: {
        ConsoleTestReporter,
        HtmlConsoleOutput,
    },
} = require( '../src' );


module.exports = {
    console: ( program, stdout ) => module.exports.common(
        program,
        stdout,
        TestRunner(
            ConsoleTestReporter( stdout ),
            program
        )
    ),

    browser: ( program, stdout ) => module.exports.common(
        program,
        stdout,
        AsyncTestRunner(
            ConsoleTestReporter.use( HtmlConsoleOutput )( stdout ),
            program
        )
    ),

    common: ( program, stdout, runner ) =>
    {
        const reader = YamlTestReader
            .use( DateResolver )
            .use( ConstResolver( program ) )
            ( yaml_reader, TestCase );

        // XXX: work around issue with consts not being initialized ahead of
        // time (initialized during actual rating...!)
        program.rater( {}, false );

        return yaml => new Promise( ( resolve, reject ) =>
        {
            try
            {
                const cases = reader.loadCases( yaml );

                resolve( runner.runTests( cases ) );
            }
            catch ( e )
            {
                reject( e );
            }
        } );
    },
};
