/**
 * TAME compiler fontend
 *
 *  Copyright (C) 2016, 2018 R-T Specialty, LLC.
 *
 *  This file is part of the Liza Data Collection Framework
 *
 *  liza is free software: you can redistribute it and/or modify
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
 *
 * Starting the JVM and compiling the XSLT stylesheets incurs a prohibitive
 * startup cost when compiling individual files (as is done with make).
 * This works around that issue.
 *
 * Note that this current implemention does not allow for parallel builds.
 * That will change.
 */

package com.lovullo.dslc;

import java.io.*;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Map;
import java.util.HashMap;
import javax.xml.XMLConstants;
import javax.xml.transform.stream.StreamSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.*;
import javax.xml.validation.*;
import javax.xml.transform.sax.SAXTransformerFactory;
import org.xml.sax.SAXException;
import org.xml.sax.SAXNotRecognizedException;
import org.xml.sax.SAXNotSupportedException;


// TODO: Decouple from rater/ path assumptions
public class DslCompiler
{
    private static class _DslCompiler
    {
        private Validator _xsd;
        private HashMap<String,Transformer> _xsl;
        private Path _pathRoot;


        public _DslCompiler()
        {
            _xsd  = _createXsd();
            _xsl  = new HashMap<String,Transformer>();
        }


        public void compile(
            Source doc,
            String cmd,
            String src,
            String dest,
            HashMap<String,String> params
        ) throws Exception
        {
            if ( cmd.equals( "validate" ) )
            {
                _xsd.validate( doc );
                return;
            }
            else if ( cmd.equals( "rm" ) )
            {
                // remove file (purposely uncaught)
                ( new File( src ) ).delete();

                return;
            }

            if ( dest.equals( "" ) )
            {
                System.err.printf(
                    "fatal: no destination path provided\n"
                );

                System.exit( 4 );
            }

            // root path of TAME
            _pathRoot = Paths.get( "rater/tame" ).toRealPath();

            // transform to dest
            File destfile = new File( dest );
            try
            {
                _transform(
                    src,
                    doc,
                    cmd,
                    new StreamResult( new File( dest ) ),
                    params
                );

                // TODO: more unique identifier
                System.err.println( "DONE 0 " + dest );
            }
            catch ( Exception e )
            {
                // delete the output file; it's garbage
                destfile.delete();

                System.err.println( "DONE 1 " + dest );

                // be verbose and unprofessional.
                throw e;
            }
        }


        private void _transform(
            String src,
            Source doc,
            String cmd,
            StreamResult dest,
            HashMap<String,String> params
        ) throws Exception
        {
            // load the stylesheet if it has not been already (we load lazily in
            // case the stylesheet is never needed)
            if ( !( _xsl.containsKey( cmd ) ) )
            {
                _xsl.put( cmd, _createXslt( cmd ) );
            }

            // since XSL's abstraction does not provide a means to retrieve the
            // document path (nor does it make sense to), we will pass it in
            // ourselves, stripped of the file extension
            String srcpkg = src.substring( 0, src.lastIndexOf( '.' ) );

            // similarily, quickly resolve the relative root path
            Integer dircount = srcpkg.replaceAll( "[^/]", "" ).length();
            String relroot   = new String( new char[ dircount ] ).replace( "\0", "../" );

            Transformer t = _xsl.get( cmd );
            t.setParameter( "__path-root", _pathRoot.toString() );
            t.setParameter( "__srcpkg", srcpkg );
            t.setParameter( "__relroot", relroot );
            t.setParameter( "__rseed", (int)( Math.random() * 10e6 ) );

            _setTemplateParams( t, params );

            t.transform( doc, dest );
        }


        private void _setTemplateParams(
            Transformer t,
            HashMap<String,String> params
        ) throws Exception
        {
            for ( Map.Entry<String, String> param : params.entrySet() )
            {
                t.setParameter( param.getKey(), param.getValue() );
            }
        }


        private Validator _createXsd()
        {
            final SchemaFactory factory = SchemaFactory.newInstance(
                XMLConstants.W3C_XML_SCHEMA_NS_URI
            );

            // we must disable Unique Particle Attribution (UPC) checking; the
            // validator used during development did not check for this and it
            // currently does not pass this test (note that disabling this also
            // improves the speed of the validator)
            try
            {
                factory.setFeature(
                    "http://apache.org/xml/features/validation/schema-full-checking",
                    false
                );
            }
            catch ( Exception e )
            {
                System.err.println(
                    "fatal: cannot disable UPA checking; " +
                    e.getMessage()
                );

                System.exit( 1 );
            }

            try
            {
                final Schema schema  =
                    factory.newSchema( new File( "rater/rater.xsd" ) );

                return schema.newValidator();
            }
            catch ( SAXException e )
            {
                System.err.printf(
                    "fatal: %s\n",
                    e.getMessage()
                );

                System.exit( 1 );
            }

            return null;
        }


        private Transformer _createXslt( String src )
        {
            try
            {
                final TransformerFactory factory = TransformerFactory.newInstance(
                    "net.sf.saxon.TransformerFactoryImpl", null
                );

                final Source xsl = new StreamSource( "rater/" + src + ".xsl" );

                return factory.newTransformer( xsl );
            }
            catch ( Exception e )
            {
                System.err.printf(
                    "fatal: compilation failed; %s\n",
                    e.getMessage()
                );

                System.exit( 2 );
            }

            return null;
        }
    }



    public static void main( String[] args ) throws Exception
    {
        BufferedReader stdin = new BufferedReader(
            new InputStreamReader( System.in )
        );

        String src = ( args.length > 0 ) ? args[0] : "";

        _DslCompiler dslc = new _DslCompiler();

        try
        {
            if ( src != "" )
            {
                compileSrc( dslc, src );
            }
            else
            {
                while ( ( src = stdin.readLine() ) != null )
                {
                    compileSrc( dslc, src );
                }
            }
        }
        catch ( IOException e )
        {
            System.err.println(
                "fatal: I/O error while reading input files: " +
                e.getMessage()
            );

            System.exit( 1 );
        }
        catch ( Exception e )
        {
            System.err.printf(
                "fatal: `%s': %s\n",
                src,
                e.getMessage()
            );

            // generic exception..ruh roh
            throw e;
        }
    }


    private static void compileSrc( _DslCompiler dslc, String cmdline ) throws Exception
    {
        System.err.println( cmdline );
        String[] args = cmdline.split( " " );
        String   dest = "";

        if ( args.length < 2 )
        {
            System.err.printf( "fatal: invalid command: %s\n", cmdline );
            System.exit( 3 );
        }
        else if ( args.length >= 3 )
        {
            dest = args[2];
        }

        String  cmd = args[0];
        String  src = args[1];

        HashMap<String,String> params = _getXslParams( args );

        Source doc = new StreamSource( src );
        dslc.compile( doc, cmd, src, dest, params );
    }


    private static HashMap<String,String> _getXslParams( String[] args )
        throws Exception
    {
        HashMap<String,String> params = new HashMap<String,String>();

        for ( int i = 3; i < args.length; i++ )
        {
            String[] keyval = args[ i ].split( "=" );

            if ( keyval.length < 2 )
            {
                throw new Exception(
                    "Invalid template param assignment: " +
                    args[ i ]
                );
            }

            params.put( keyval[ 0 ], keyval[ 1 ] );
        }

        return params;
    }
}
