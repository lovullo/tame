/**
 * Environment-specific runner initialization
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

const yaml_reader = require( 'js-yaml' );

const {
    TestCase,
    TestRunner,

    reader: {
        ConstResolver,
        DateResolver,
        YamlTestReader
    },

    reporter: {
        ConsoleTestReporter
    },
} = require( '../src' );


module.exports = {
    console: ( program, stdout ) =>
    {
        const runner = TestRunner(
            ConsoleTestReporter( stdout ),
            program
        );

        const reader = YamlTestReader
            .use( DateResolver )
            .use( ConstResolver( program ) )
            ( yaml_reader, TestCase );

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
