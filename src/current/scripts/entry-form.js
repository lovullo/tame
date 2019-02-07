/**
 * Summary page program
 *
 *  Copyright (C) 2014-2019 Ryan Specialty Group, LLC.
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
 * This file is used for direct interaction with the rater for testing purposes.
 * As such, much of it is a rushed implementation; it's a bit of a kluge and
 * could use some refactoring.
 *
 * Also, it is terriby stateful and difficult to work with. I have the utmost
 * confidence in your ability to look away and pretend you never saw this
 * script.
 */

// intentionally global; developers can override
var program    = document.location.pathname.match( '/raters/(.*?)/' )[1],
    submit_url = '/raters/submit-test.php?program=' + program,
    supplier   = rater.supplier,
    prior_url  = '/raters/submit-test.php?retrieve=' + supplier +
        '&program=' + program,
    qdata_host = 'dev';

// last YAML test case results
let yaml_results = [];

var client = ( function()
{
    // URL to which quote/result submissions should be POSTed
    var form           = document.querySelector( 'form.entry-form' ),
        final_prem     = form.querySelector( '.final-premium' ),
        final_accept   = form.querySelector( '.final-accept' ),
        final_comments = form.querySelector( '.final-comments' ),
        voi            = document.getElementById( 'voi-list' ),
        coview         = document.getElementById( 'class-overview-list' ),

        final_good = document.getElementById( 'final-accept-good' ),
        final_bad  = document.getElementById( 'final-accept-bad' ),

        load_prior = document.getElementById( 'load-prior' ),

        workstatus = null,

        valspan     = {},
        bucket      = {},
        rate_result = {},

        // used to overwrite existing test cases rather than create a new
        save_id = '',
        prior_result,

        rate_callback = function() {},

        // whether to ignore user input (do not put in bucket)
        ignore_input = false;


    populateBucket();


    function setWorkStatus( message )
    {
        if ( workstatus === null )
        {
            workstatus = document.createElement( 'div' );
            workstatus.id = 'workstatus';

            document.body.appendChild( workstatus );
        }

        workstatus.innerHTML = message;
        workstatus.className = ( message ) ? 'show' : '';
    }

    function populateBucket()
    {
        Array.prototype.slice.call( form.querySelectorAll( '[name]' ) ).forEach(
            function( field )
            {
                var name = field.name.replace( /\[\]$/, '' );

                if ( !name )
                {
                    return;
                }

                // if the name does not match, then we removed the square
                // brackets, meaning that this is a set
                bucket[ name ] = ( name === field.name )
                    ? +field.value
                    : [ +field.value ];

                updateParamTestcaseDfn( name );
            }
        );
    }


    function overrideBucket( boverride )
    {
        for ( var name in boverride )
        {
            bucket[ name ] = boverride[ name ];
        }

        emptyBucket();
    }


    function removeEntryFocus()
    {
        form.className = form.className.replace( /\bfocus\b/, '' );
    }

    document.body.addEventListener( 'mouseup', function( e )
    {
        var overform = hasParent( form, e.target );

        if ( overform === false )
        {
            removeEntryFocus();
        }
    } );

    form.addEventListener( 'reset', function()
    {
        if ( ignore_input )
        {
            return;
        }

        // wait until *after* reset
        setTimeout( function()
        {
            clearTestCase();

            bucket = {};
            populateBucket();
            clearSummaryPremium();

            setWorkStatus();
        }, 0 );
    } );

    form.addEventListener( 'mouseover', function()
    {
        showEntryForm();
    } );


    function clearTestCase()
    {
        save_id      = '';
        prior_result = undefined;

        // clear prior class from body
        document.body.className = document.body.className.replace(
            /\bprior\b/, ''
        );
    }


    function setTestCase( id, result )
    {
        save_id      = ''+( id );
        prior_result = result;

        // this really should be set...
        if ( !( prior_result.vars ) )
        {
            prior_result.vars = {};
        }

        // add prior class name to body
        document.body.className += ' prior';
    }


    function showEntryForm()
    {
        if ( form.className.match( /\bfocus\b/ ) )
        {
            return;
        }

        form.className += ' focus';
    }


    // on field change, update bucket
    form.addEventListener( 'change', function( e )
    {
        if ( ignore_input )
        {
            return;
        }

        // if we changed something, then the displayed premium (if any) must be
        // invalidated
        clearSummaryPremium();

        var target = e.target,
            name   = target.name.replace( /\[\]$/, '' ),
            value  = +target.value.trim();

        if ( !name )
        {
            return;
        }

        // if this is a set, we want to store every value
        if ( name !== target.name )
        {
            var toarr = Array.prototype.slice;

            // retrieve all the rows
            var rows = toarr.call(
                target.parentElement.parentElement.parentElement
                    .querySelectorAll( '.entry-row' )
            );

            // determine if we're working with a matrix
            var matrix = /\bmatrix\b/.test( rows[ 0 ].className );

            value = [];
            rows.forEach( function( row, i )
            {
                var ref = value;

                // for matricies, add value to a sub-array; vectors, just keep
                // appending to the original array
                if ( matrix )
                {
                    ref = value[ i ] = [];
                }

                // add each value
                toarr.call( row.querySelectorAll( '[name]' ) ).forEach(
                    function( node )
                    {
                        ref.push( +node.value.trim() );
                    }
                );
            } );
        }

        bucket[ name ] = value;

        // update entry dfn
        updateParamTestcaseDfn( name, value );
    } );

    // update screen on submit
    form.addEventListener( 'submit', function( e )
    {
        // do not submit the form
        e.preventDefault();
        rate( bucket );
    } );

    form.addEventListener( 'click', function( e )
    {
        if ( e.target.className === 'entry-add' )
        {
            addRow( e.target.parentElement, e.target );
            e.preventDefault();
        }
        else if ( e.target.className === 'entry-rm' )
        {
            removeColumn( e.target );
            e.preventDefault();
        }
        else if ( e.target.className === 'entry-add-matrix' )
        {
            addColumn( e.target );
            e.preventDefault();
        }
    } );



    final_good.addEventListener( 'click', function( e )
    {
        e.preventDefault();

        showFinalComments( true, function( comment, _, waiting )
        {
            var prem = rate_result.premium;

            hideFinalAccept();
            submitQuote( bucket, rate_result, comment, true, waiting, prem, save_id );
        } );
    } );

    final_bad.addEventListener( 'click', function( e )
    {
        e.preventDefault();

        showFinalComments( false, function( comment, expect, waiting )
        {
            hideFinalAccept();
            submitQuote( bucket, rate_result, comment, false, waiting, expect, save_id );
        } );
    } );


    function updateParamTestcaseDfn( name, value )
    {
        const dfn_element = getParamTestcaseDfnElement( name );

        if ( dfn_element === undefined )
        {
            return;
        }

        value = value || bucket[ name ];

        const dfn = name + ': ' + JSON.stringify( value );
        dfn_element.innerText = dfn;
    }


    function getParamTestcaseDfnElement( name )
    {
        return document.querySelectorAll(
            '#param-input-' + name + ' > .entry-testcase-dfn'
        )[ 0 ];
    }


    function showFinalComments( looksgood, callback )
    {
        final_comments.className += ' show';

        var submit = document.getElementById( 'final-submit' ),
            cancel = document.getElementById( 'final-cancel' ),

            expect_container = document.getElementById(
                'final-expect-container'
            ),

            submit_new = document.getElementById( 'final-submit-new' ),

            listener;

        // we do not care about the expected value if the premium looks good
        expect_container.style.display = ( looksgood )
            ? 'none'
            : 'inline';

        // if a test case is set, give them the option to clear it and submit it
        // as a new test case
        submit_new.style.display = ( save_id )
            ? 'inline'
            : 'none';

        // make it very clear what the user is about to do
        submit.innerHTML = ( save_id )
            ? 'Update Existing Test Case'
            : 'Submit';

        // we won't use addEventListener becuase we only want one event to be
        // attached
        submit.onclick = function( e )
        {
            e.preventDefault();

            var comments = document.getElementById( 'final-comments' ),
                expected = document.getElementById( 'final-expected' ),
                waiting  = document.getElementById( 'final-waiting' );

            callback(
                comments.value,
                +( expected.value.replace( /^\$/, '' ) ),
                !!waiting.checked
            );

            rmclass( final_comments, 'show' );
        };

        submit_new.onclick = function( e )
        {
            e.preventDefault();

            // clear save id and trigger normal submit
            save_id = '';
            submit.onclick( e );
        };

        cancel.onclick = function( e )
        {
            e.preventDefault();
            rmclass( final_comments, 'show' );
        };

        // give focus to final comments
        document.getElementById( 'final-comments' ).focus();
    }


    function hideFinalAccept()
    {
        // replace all shows since there may be multiple
        final_accept.className = final_accept.className.replace(
            /\bshow\b/,
            ''
        );
    }


    function getXhrJsonSync( method, url, data )
    {
        var xhttp = new XMLHttpRequest();

        xhttp.open( method, url, false );

        if ( method.toLowerCase() === 'post' )
        {
            xhttp.setRequestHeader( 'Content-type',
                'application/x-www-form-urlencoded'
            );
        }

        xhttp.send( ( data ) ? 'data=' + JSON.stringify( data ) : null );

        if ( xhttp.status !== 200 )
        {
            throw Error( 'Submit failed; status: ' + xhttp.status );
        }

        // this will fail if the response is crap, but will be caught by the
        // exception
        return JSON.parse( xhttp.responseText );
    }


    function getXhrJson( method, url, data, callback )
    {
        var xhttp = new XMLHttpRequest();
        xhttp.open( method, url, true );

        if ( method.toLowerCase() === 'post' )
        {
            xhttp.setRequestHeader( 'Content-type',
                'application/x-www-form-urlencoded'
            );
        }

        xhttp.onload = function()
        {
            if ( xhttp.status !== 200 )
            {
                callback( null,
                    Error( 'Submit failed; status: ' + xhttp.status )
                );
                return;
            }

            callback( JSON.parse( xhttp.responseText ) );
        }

        xhttp.send( ( data ) ? 'data=' + JSON.stringify( data ) : null );
    }


    function submitQuote(
        bucket, result, comment, looksgood, waiting, expected, caseid, success_callback
    )
    {
        // we don't want to modify the original result (could use
        // Object.create() here, but they may be using IE)
        var tmpresult = function() {};
        tmpresult.prototype = result;

        // it is absolutely pointless to store debug information since the ids
        // change at any time and are dependent on the XSL processor
        var submit_result = new tmpresult();

        // so that it's property serialized
        for ( var name in result )
        {
            submit_result[ name ] = result[ name ];
        }

        // we do not need the debug information (there's a lot of it and it
        // changes frequently)
        submit_result.debug = undefined;

        // nor do we need constants (especially large ones)!
        submit_result.consts = undefined;

        var data  = {
            bucket:    bucket,
            result:    submit_result,
            looksgood: !!looksgood,
            waiting:   !!waiting,
            comment:   encodeURIComponent( comment ),
            expected:  expected,
            supplier:  supplier,

            // will cause an existing test case to be overwritten, if set
            caseid: caseid,
        };

        getXhrJson( 'POST', submit_url, data, function( response, err )
        {
            // check the response of the actual save (just because we
            // got a HTTP 200 doesn't mean we successfully saved to the
            // server; we could have also hit the wrong page
            // (misconfigured)!)
            if ( err )
            {
                alert(
                    'Ah, crap! Quote submission failed! Contact IT before ' +
                    'you do anything else.\n\n' +
                    'Here are the boring details:\n' +
                        '    ' + err.message
                );

                return;
            }

            success_callback && success_callback();
        } );
    }



    function addRow( parent, before )
    {
        before = before || parent.querySelector( '.entry-add' );

        // get the row to duplicate
        var dup = parent.querySelector( '.entry-row' )
            .cloneNode( true );

        parent.insertBefore( dup, before );

        // trigger change so that its value can be recorded
        triggerChange( dup.querySelector( '[name]' ) );
    }


    function addColumn( event_target )
    {
        // get the field to duplicate
        var dup = event_target.parentElement.querySelector( '.entry-field' )
            .cloneNode( true );

        event_target.parentElement.insertBefore( dup, event_target );

        // trigger change so that its value can be recorded
        triggerChange( dup.querySelector( '[name]' ) );
    }


    function removeColumn( event_target )
    {
        var rm        = event_target.parentElement,
            parent    = rm.parentElement,
            rowParent = parent.parentElement,

            rows = rowParent.querySelectorAll( '.entry-row' ).length,
            cols = parent.querySelectorAll( '.entry-field' ).length;

        // do not remove last column of last row
        if ( ( rows + cols ) === 2 )
        {
            return;
        }

        // remove the element
        parent.removeChild( rm );

        // if there are no more columns, remove the row
        if ( cols === 1 )
        {
            rowParent.removeChild( parent );
        }

        // re-gather values in bucket to accomodate missing value (we can do so
        // simply by triggering a change on one of the elements of the same
        // name)
        triggerChange( rowParent.querySelector( '[name]' ) );
    }


    function triggerChange( element )
    {
        if ( !element )
        {
            return;
        }

        // create change event
        var event = document.createEvent( 'Event' );
        event.initEvent( 'change', true, true );

        // trigger event
        element.dispatchEvent( event );
    }


    function rate( args, showresults, exception )
    {
        showresults = ( showresults === undefined ) ? true : !!showresults;
        exception   = !!exception;

        var rater = window.rater;

        if ( !( window.rater ) )
        {
            alert( 'fatal: rater unavailable.' );
            return;
        }

        setWorkStatus( 'Performing rating...' );

        try
        {
            var result = rater( args );

            // XXX: ewwww
            rate_result = result;

            if ( !( showresults ) )
            {
                return;
            }

            // log result to the console in case we want to peeky peeky
            console.log( result );

            rate_callback( result );

            setWorkStatus( 'Updating premium...' );
            updateSummaryPremium( result.premium );

            // VOIs are referenced immediately, so render them first
            updateVois( result.vars, function()
            {
                // classes are faster to process than the other summary values
                updateSummaryClasses(
                    result.classes,
                    result.vars,
                    undefined,

                    function()
                    {
                        updateSummaryValues( result.vars );
                    }
                );
            } );
        }
        catch ( e )
        {
            setWorkStatus( 'Rating error occurred.' );

            console && console.log( e );

            if ( exception )
            {
                throw e;
            }
            else
            {
                alert( 'fatal: ' + e.message );
            }
        }
    }


    function updateSummaryPremium( premium )
    {
        final_prem.innerHTML = premium;
        final_prem.className += ' show';

        final_accept.className += ' show';

        setPlaceholderValue( 'yields_premium', '', premium );
    }


    function clearSummaryPremium()
    {
        final_prem.innerHTML = '';

        rmclass( final_prem, 'show' );
        rmclass( final_accept, 'show' );
    }


    function getValueDisplay( value )
    {
        if ( Array.isArray( value ) )
        {
            return joinValues( value );
        }

        return ( value === undefined )
            ? ''
            : ''+( value );
    }


    function updateVois( vars, callback )
    {
        setWorkStatus( 'Processing VOIs...' );
        voi.innerHTML = '';

        var queue = [];
        for ( var name in vars )
        {
            queue.push( name );
        }

        var vois = {},
            qlen = queue.length,
            i    = qlen;

        dequeueSetsOf( 10, function( c )
        {
            if ( i-- === 0 )
            {
                // display the VOIs
                processVois( vois );
                document.getElementById( 'voi-container' ).className += ' show';

                setWorkStatus();
                callback && callback();

                return;
            }

            setWorkStatus(
                'Processing VOIs (' +
                Math.floor( ( ( qlen - i ) / qlen ) * 100 ) +
                '%)...'
            );

            var name  = queue[ i ],
                value = vars[ name ];

            if ( value
                && /^prem|^min|^surcharge|^cov(erage)?|^credit|^percent|^factor|^rate|Prem|[tT]otal/
                    .test( name )
                && !( /^_/.test( name ) )
            )
            {
                var display = getValueDisplay( value ),
                    prior   = ( prior_result && prior_result.vars[ name ] )
                        ? getValueDisplay( prior_result.vars[ name ] )
                        : '';

                // update values of interest (voi)
                if ( display !== '[]' )
                {
                    vois[ name ] = [ display, prior ];
                }
            }

            // continue
            c();
        } )();
    }


    function updateSummaryValues( vars, placeid, callback )
    {
        var queue = [];

        for ( var name in vars )
        {
            queue.push( name );
        }

        var qlen = queue.length;

        // repaint frequently; this is intensive
        dequeueSetsOf( 10, function( c )
        {
            if ( queue.length === 0 )
            {
                setWorkStatus();
                return;
            }

            name = queue.pop();

            setWorkStatus(
                'Formatting summary values (' +
                Math.floor( ( ( qlen - queue.length ) / qlen ) * 100 ) +
                '%)...'
            );

            var value   = vars[ name ],
                display = getValueDisplay( value ),
                prior   = ( prior_result && prior_result.vars[ name ] )
                    ? getValueDisplay( prior_result.vars[ name ] )
                    : '';


            setPlaceholderValue( name, '', display );

            if ( prior )
            {
                setPlaceholderValue( name, '-prior', prior );
            }

            setLetListPlaceholders( name, display, prior );

            // continue
            c();
        } )();
    }


    function dequeueSetsOf( n, c )
    {
        return function dq( i )
        {
            i = i || 0;

            c( function()
            {
                if ( i === 0 )
                {
                    setTimeout( function()
                    {
                        dq( n )
                    }, 0 );
                }
                else
                {
                    dq( i - 1 );
                }
            } );
        }
    }


    function processVois( vois )
    {
        // add the vois to the screen in the proper order (reversed)
        var i = window.voi_order.length;
        while ( i-- )
        {
            var data  = window.voi_order[ i ],
                name  = data[ 0 ],
                depth = data[ 1 ],
                href  = data[ 2 ];

            if ( vois[ name ] )
            {
                var voi = vois[ name ];
                addVoi( name, voi[ 0 ], voi[ 1 ], href, depth );
            }
        }
    }


    function addVoi( name, value, prior, href, depth )
    {
        depth = depth || 0;

        // if the VOI has a value other than 0 (our poor-man check is using a
        // regex to remove anything and see if we have a non-empty string left)
        if ( ( value.replace( /[\[\]0,]/g, '' ) === '' )
            && ( prior.replace( /[\[\]0,]/g, '' ) === '' )
        )
        {
            return;
        }

        var depthstr = '',
            i        = depth;
        while ( i-- )
        {
            if ( i === 0 )
            {
                depthstr += '|-';
            }

            depthstr += '&nbsp;&nbsp;';
        }

        // if href is not given, then use name
        href = href || name;

        // got lazy.
        var tr = document.createElement( 'tr' );
        tr.className = 'depth' + depth;
        tr.innerHTML = (
            '<td>' +
                '<a href="#' + href + '">' +
                    depthstr + name +
                '</a>' +
            '</td>' +
            '<td>' + depthstr.replace( /-/, '' ) + value + '</td>' +
            ( ( !prior ) ? '' :
                '<td class="prior">' + prior + '</td>'
            )
        );

        tr.addEventListener( 'click', function( e )
        {
            // ignore link clicks
            if ( e.target.nodeName === 'A' )
            {
                return;
            }

            var val = JSON.parse( value );
            if ( Array.isArray( val ) )
            {
                var t = 0;
                for ( var i in val )
                {
                    t += val[ i ];
                }

                val = t;
            }

            voiPainterAdd( tr, val );
        } );

        voi.appendChild( tr );
    }


    function addClassOverview( name, value )
    {
        var prior = ( prior_result && prior_result.vars[ name ] )
            ? getValueDisplay( prior_result.vars[ name ] )
            : '';

        // got lazy.
        var tr = document.createElement( 'tr' );
        tr.innerHTML = (
            '<td>' +
                '<a href="#:class:' + name + '">' +
                    name +
                '</a>' +
            '</td>'
        );

        coview.appendChild( tr );
    }


    function joinValues( values )
    {
        var ret = '[';

        if ( Array.isArray( values[ 0 ] ) )
        {
            var subvals = [];

            for ( var i in values )
            {
                subvals.push( joinValues( values[ i ] ) );
            }

            ret += subvals.join( ', ' );
        }
        else
        {
            ret += ( Array.isArray( values ) )
                ? values.join( ', ' )
                : values;
        }

        return ret + ']';
    }


    function updateSummaryClasses( classes, vars, placeid, callback )
    {
        coview.innerHTML = '';

        var queue = [];

        for ( var name in classes )
        {
            queue.push( name );
        }

        var qlen = queue.length;

        dequeueSetsOf( 10, function( c )
        {
            if ( queue.length === 0 )
            {
                setWorkStatus();
                callback && callback();

                return;
            }

            var name = queue.pop();

            // hide internal classes ("-" prefix)
            if ( /^-/.test( name ) )
            {
                return c();
            }

            setWorkStatus(
                'Formatting class summary values (' +
                Math.floor( ( ( qlen - queue.length ) / qlen ) * 100 ) +
                '%)...'
            );

            // output the classification and the total premium for the class
            setPlaceholderValue( 'class-' + name, placeid,
                (
                    ''+( classes[ name ] ) +
                    ' -> ' +
                    vars[ name ]
                ),
                classes[ name ]
            );

            if ( prior_result
                && prior_result.classes
                && prior_result.classes[ name ]
            )
            {
                // XXX: duplicate
                setPlaceholderValue( 'class-' + name, '-prior',
                    (
                        ''+( prior_result.classes[ name ] ) +
                        ' -> ' +
                        prior_result.vars[ name ]
                    ),
                    classes[ name ]
                );
            }

            // if this class was a match, add it to the overview with its
            // accumulator value
            if ( classes[ name ] )
            {
                addClassOverview( name, vars[ name ] );
            }

            c();
        } )();

        document.getElementById( 'class-overview' ).className += ' show';
    }


    function updateSummaryDebug( debug, parent, callback )
    {
        var queue = [];

        // do nothing if debug data is not yet available
        if ( !debug )
        {
            return;
        }

        // loop through each element on the DOM, *not* each debug id returned to
        // us, since we want to clear any that may be missing
        Array.prototype.slice.call( parent.querySelectorAll( '.debugid' ) )
            .forEach( function( element )
            {
                queue.push( element.id );
            } );

        var qlen = queue.length;

        dequeueSetsOf( 10, function( c )
        {
            if ( queue.length === 0 )
            {
                setWorkStatus();
                callback && callback();

                return;
            }

            setWorkStatus(
                'Processing breakdown values (' +
                Math.floor( ( ( qlen - queue.length ) / qlen ) * 100 ) +
                '%)...'
            );

            var id  = queue.pop(),
                did = id.replace( /^ubd-/, '' );

            try
            {
                setPlaceholderValue( id, '', ( debug[ did ] )
                    ? JSON.stringify( debug[ did ] )
                    : ''
                );
            }
            catch ( e )
            {
                console.error(
                    'Debug (stringify debug ' + did + ' ): ' +
                    e.message
                );
            }

            c();
        } )();
    }


    var getPlaceholder = ( function()
    {
        var domcache = {};

        function getPlaceholder( name, placeid )
        {
            var classname = ( 'entry-value' + ( placeid || '' ) );

            var current = domcache[ name + placeid ];
            if ( current )
            {
                return current;
            }

            // ignore system vars
            if ( name.match( /^___/ ) )
            {
                return null;
            }

            var parent = document.getElementById( name );

            if ( !parent )
            {
                return null;
            }

            var legend = parent.getElementsByTagName( 'legend' ),
                dest   = ( legend.length ) ? legend[ 0 ] : parent;

            var element = document.createElement( 'span' );
            element.className = classname;
            dest.appendChild( element );

            // rather than re-scanning the DOM each time
            domcache[ name + placeid ] = element;

            return element;
        }

        return getPlaceholder;
    } )();


    function setPlaceholderValue( name, placeid, value, hasval )
    {
        var p = getPlaceholder( name, placeid );
        if ( p === null )
        {
            return;
        }

        p.innerHTML = value;

        // do not handle prior flagging
        if ( placeid === '-prior' )
        {
            return;
        }

        // get fieldset
        var fs = p.parentNode.parentNode;
        if ( fs.nodeName === 'FIELDSET' )
        {
            fs.className = fs.className.replace( /\Bhasval\B/, '' );
            if ( ( hasval !== undefined && hasval )
                // progressively more time-consuming checks
                || ( ( hasval === undefined )
                    && value
                    && +value !== 0
                    && value.replace( /[\[\],0]/g, '' )
                )
            )
            {
                fs.className += ' hasval';
            }
        }
    }


    function setLetListPlaceholders( name, value, prior )
    {
        if ( name.match( /^___/ ) )
        {
            return;
        }

        // certainly room for improvement here (especially performance-wise),
        // but this is a quick implementation
        var elements = document.querySelectorAll( '.letlist-' + name );
        Array.prototype.slice.call( elements ).forEach( function( element )
        {
            if ( !( element.id ) )
            {
                // prefix with alpha so as not to cause a syntax error on query
                element.id = 'll' + Math.floor(
                    ( new Date() ).getTime() * Math.random()
                );
            }

            setPlaceholderValue( element.id, '', value );

            // include prior values, if available
            if ( prior )
            {
                setPlaceholderValue( element.id, '-prior', prior );
            }
        } );
    }


    function rmclass( element, name )
    {
        element.className = element.className.replace(
            new RegExp( '\\b' + name + '\\b', 'g' ),
            ''
        );
    }


    function hasParent( parent, element )
    {
        var parentElement = element.parentElement;

        if ( parentElement === parent )
        {
            return true;
        }

        return ( parentElement )
            ? hasParent( parent, parentElement )
            : false;
    }


    // XXX: This is a mess. THIS IS WHAT TIME CONSTRAINTS DO TO CODE QUALITY!
    // LET ME HACK IN PEACE! >:@ (What? Unconstrained development is a fantasy?
    // Is unlimited time unreasonable? Phf. Maybe that Time Weaver frog person
    // knows how to help with that. If you don't know that reference and you're
    // in here hacking this code, then that implies that you are new; it then
    // begs the question: why has it persisted for so long!!! Of course it has,
    // though. That's how TODOs/XXXs work: they don't get fixed; they just turn
    // text in your editor pretty [obnoxious] colors.)
    function resetFields()
    {
        // XXX: gahhhhhh!!!!!!!
        ignore_input = true;

        function rowquery( name )
        {
            return document.querySelectorAll(
                '#param-input-' + name + ' > .entry-row'
            );
        }

        for ( var field in bucket )
        {
            // may happen if we're loading data from another source
            if ( bucket[ field ] === undefined )
            {
                continue;
            }

            var fdata    = bucket[ field ],
                elements = rowquery( field ),

                length = ( fdata.length > elements.length )
                    ? fdata.length
                    : elements.length;

            if ( elements.length === 0 )
            {
                continue;
            }

            // not everything is an array of values
            if ( Array.isArray( fdata ) )
            {
                // add/clear fields on the form as necessary to accomdate bucket
                // data
                for ( var i = 0; i < length; i++ )
                {
                    // field exists in bucket but not on the form
                    if ( ( fdata[ i ] !== undefined ) && !( elements[ i ] ) )
                    {
                        addRow( elements[ 0 ].parentNode );
                    }
                    // field exists on form but not in the bucket
                    else if ( elements[ i ] && ( fdata[ i ] === undefined ) )
                    {
                        // TODO: remove field instead
                        elements[ i ].querySelector( '[name]' ).value = '';
                    }

                    // if we have a matrix of values, we must also add columns
                    // for each
                    if ( Array.isArray( fdata[ i ] ) )
                    {
                        var element = rowquery( field )[ i ],
                            cols    = element.querySelectorAll(
                                '.entry-field'
                            ),

                            len = ( fdata[ i ].length > cols.length )
                                ? fdata[ i ].length
                                : cols.length;

                        // check each column
                        for ( var j = 0; j < len; j++ )
                        {
                            if ( ( fdata[ i ][ j ] !== undefined )
                                && !( cols[ j ] )
                            )
                            {
                                // re-query in case we just added a row
                                addColumn( element.querySelector(
                                    '.entry-add-matrix'
                                ) );
                            }
                            else if ( cols[ j ]
                                && ( fdata[ i ][ j ] === undefined )
                            )
                            {
                                // TODO: remove field instead
                                cols[ i ].querySelector( '[name]' ).value = '';
                            }
                        }
                    }
                }
            }

            updateParamTestcaseDfn( field );
        }

        form.reset();
        ignore_input = false;
    }


    function emptyBucket()
    {
        resetFields();

        // prevent form updates from propagating to the bucket
        ignore_input = true;

        for ( var field in bucket )
        {
            var fdata = bucket[ field ];

            // not everything is an array; if not, simply set the value and move
            // on
            if ( !( Array.isArray( fdata ) ) )
            {
                var element = document.querySelector(
                    '[name="' + field + '"]'
                );

                if ( element )
                {
                    element.value = fdata;
                }

                continue;
            }

            var elements = document.querySelectorAll(
                '[name="' + field + '[]"]'
            );

            var total = 0;
            for ( var i = 0, l = fdata.length; i < l; i++ )
            {
                if ( !( elements[ i ] ) )
                {
                    continue;
                }

                // if a matrix, update each value
                if ( Array.isArray( fdata[ i ] ) )
                {
                    for ( var j = 0, jl = fdata[ i ].length; j < jl; j++ )
                    {
                        elements[ total++ ].value = fdata[ i ][ j ];
                    }
                }
                else
                {
                    // not a matrix
                    elements[ total++ ].value = fdata[ i ];
                }
            }

            updateParamTestcaseDfn( field );
        }

        // re-allow input
        ignore_input = false;
    }


    function getUserFromHostname( hostname )
    {
        // strip off any domain, remove number from username and strip anything
        // after a dash (e.g. gerwitm-ubuntu2.lovullo.local => gerwitm)
        return hostname.split( '.' )[ 0 ].replace( /[0-9]+$/, '' )
            .split( '-' )[ 0 ];
    }


    /**
     * Prior module: load prior quotes (test cases)
     *
     * Not to be confused in speech with the Friar module, which would have your
     * premiums divinely calculated and communicated through a deep meditation.
     */
    var Prior = ( function ___loadprior( dom )
    {
        var exports = {},

            // current set of loaded test cases
            curset  = {};

        var getLoadDialog = function()
        {
            // URL with fragment to automatically display this dialog
            var url = document.location.href.replace( /#.*$/, '' ) + '#prior';

            var dialog = dom.createElement( 'div' );
            dialog.id        = 'prior';
            dialog.className = 'load-dialog';
            dialog.innerHTML =
                "<h1>Load Prior Quotes</h1>" +
                "<p>" +
                    "Below is a list of all prior saved quotes; choose one " +
                    "to load it into the test area." +
                "</p>" +
                "<p>" +
                    "To load this dialog automatically on page load, you " +
                    "may use the following link: <a href=\"" + url + "\">" +
                    url + "</a>"
                "</p>";

            // re-test button
            var retest = dom.createElement( 'button' );
            retest.innerHTML = 'Regression Test';
            retest.addEventListener( 'click', function( e )
            {
                e.preventDefault();
                e.target.disabled = 'disabled';

                retestAll( function()
                {
                    e.target.disabled = '';
                } );
            } );

            // load quote number
            var loadquote = dom.createElement( 'button' );
            loadquote.innerHTML = 'Load Quote #';
            loadquote.addEventListener( 'click', function( e )
            {
                e.preventDefault();

                var qid  = prompt( 'Enter quote #:' );

                if ( !qid )
                {
                    return;
                }

                exports.hideLoad();

                loadQuote( qid, qdata_host );
            } );

            const yamlconsole = dom.createElement( 'div' );
            yamlconsole.style.display = 'none';
            yamlconsole.id            = 'yamlconsole';
            yamlconsole.addEventListener( 'click', ev =>
            {
                ev.preventDefault();

                const target = ev.target;
                if ( target.dataset.caseIndex === undefined )
                {
                    return;
                }

                loadYamlTestCase( +ev.target.dataset.caseIndex );
            } );

            const yamlbrowse = dom.createElement( 'input' );
            yamlbrowse.type          = 'file';
            yamlbrowse.style.display = 'none';
            yamlbrowse.accept        = '.yml, .yaml';
            yamlbrowse.multiple      = 'multiple';
            yamlbrowse.addEventListener( 'change', e =>
            {
                yamlconsole.style.display = '';
                yamlconsole.innerHTML     = '';

                if ( yamlbrowse.files.length === 0 )
                {
                    return;
                }

                runYamlTestCases(
                    Array.prototype.slice.call( yamlbrowse.files, 0 ),
                    createYamlRunner( yamlconsole )
                );

                yamlbrowse.value = '';

                return false;
            } );

            const yamlcases = dom.createElement( 'button' );
            yamlcases.innerHTML = 'Load YAML Test Cases';
            yamlcases.addEventListener( 'click', e =>
            {
                yamlbrowse.click();
                return false;
            } );

            dialog.appendChild( retest );
            dialog.appendChild( loadquote );
            dialog.appendChild( yamlcases );
            dialog.appendChild( yamlbrowse );
            dialog.appendChild( yamlconsole );
            dialog.appendChild( getPriorTable() );

            dom.body.appendChild( dialog );

            // reassign the function to always return the instance
            getLoadDialog = function()
            {
                return dialog;
            };

            return getLoadDialog();
        };


        /**
         * Create YAML test case runner
         *
         * @param {HTMLElement} yamlconsole element to contain runner output
         *
         * @return {function(string)} runner
         */
        const createYamlRunner = yamlconsole => require( 'progtest' )
            .env.browser(
                { rater: window.rater },
                {
                    write( str )
                    {
                        yamlconsole.innerHTML += str;
                    }
                }
            );


        /**
         * Run test cases in each YAML file FILES
         *
         * @param {Array<File>}      files  YAML files
         * @param {function(string)} runner test case runner
         *
         * @return {undefined}
         */
        const runYamlTestCases = function( files, runner )
        {
            if ( files.length === 0 )
            {
                return;
            }

            const testfile = files.shift();
            const reader   = new FileReader();

            reader.onload = ev =>
            {
                const yaml = ev.target.result;

                runner( yaml )
                    .then( results => yaml_results = results )
                    .catch( e => alert( e.message ) );

                // run for remaining files
                runYamlTestCases( files, runner );
            };

            reader.readAsBinaryString( testfile );
        };


        const loadYamlTestCase = function( caseid )
        {
            const testcase = yaml_results[ caseid ];

            if ( !testcase )
            {
                alert( 'error: No such test case: ' + caseid );
                return;
            }

            const { desc, given, expect, failures } = testcase;

            if ( !given )
            {
                alert( 'error: Malformed test case data' );
                return;
            }

            console.log( given );

            // overwrite the bucket
            bucket = given;
            emptyBucket();

            // make it obvious to the user that the data has been loaded
            clearSummaryPremium();
            showEntryForm();

            // display expected values as the "prior" values
            setTestCase( 0, { vars: expect } );

            const success = failures.length === 0;

            Prior.setPriorMessage(
                '',
                `[#${+caseid+1}] ${desc}`,
                success,
                0
            );

            // switch to test data and rate
            document.location.hash = '#test-data';
            rate( bucket );
        }


        var getPriorTable = function()
        {
            var table = dom.createElement( 'table' ),
                headings = [
                    "Date", "Description", "User", "Premium", "Expected"
                ];

            // add headings
            for ( var head in headings )
            {
                var th = dom.createElement( 'th' );
                th.innerHTML = headings[ head ];
                th.className = headings[ head ].toLowerCase();

                table.appendChild( th );
            }

            // add count
            var count = dom.createElement( 'caption' );
            count.innerHTML =
                '<b>Total Count:</b> <span class="count">0</span>';
            table.appendChild( count );

            table.clear = function()
            {
                var rows = table.querySelectorAll( 'tr' );
                for ( var i = 0; i < rows.length; i++ )
                {
                    table.removeChild( rows[ i ] );
                }
            };

            table.addRow = function( looksgood, waiting /*, ... */ )
            {
                var tr = dom.createElement( 'tr' );

                // the first argument is the id
                var id = arguments[ 0 ];
                tr.id = '_testcase_' + id;

                // the second argument will determine the row color (looksgood)
                tr.className =
                    ( ( arguments[ 1 ] )
                        ? 'good'
                        : 'bad'
                    ) +
                    ( ( arguments[ 2 ] )
                        ? ' waiting'
                        : ''
                    );

                // all other arguments will be cells
                for ( var i = 3; i < arguments.length; i++ )
                {
                    var td = dom.createElement( 'td' );
                    td.innerHTML = arguments[ i ];
                    td.className = headings[ i - 3 ].toLowerCase();

                    // first cell will contain a hyperlink for auto-loading on
                    // visit
                    if ( i === 3 )
                    {
                        var a = dom.createElement( 'a' );
                        a.href = ( '#prior/' + id );
                        a.innerHTML = td.innerHTML;

                        td.innerHTML = '';
                        td.appendChild( a );

                        a.addEventListener( 'click', function( e )
                        {
                            doLoad( id );
                        } );
                    }

                    tr.appendChild( td );
                }

                table.appendChild( tr );
            };

            table.setCount = function( count )
            {
                table.querySelector( '.count' ).innerHTML = +count;
            };

            table.mark = function( id, type )
            {
                table.querySelector( '#_testcase_' + id ).className = type;
            };

            table.changePremium = function( id, premium )
            {
                var element = table.querySelector(
                    '#_testcase_' + id + ' .premium'
                );

                // add the value and retain the previous value
                element.innerHTML = '$' + premium +
                    '<div class="prev-val">(was ' + element.innerHTML +
                    ')</div>';
            };

            table.changeComment = function( id, comment )
            {
                var element = table.querySelector(
                    '#_testcase_' + id + ' .description'
                );

                element.innerHTML = comment.replace( /\n/g, '<br />' );
            };

            function doLoad( id )
            {
                exports.hideLoad();

                // give them an indication that something is happening
                setTimeout( function()
                {
                    loadPriorTestCase( id );
                }, 0 );
            }

            // when a row is clicked, trigger the load
            table.addEventListener( 'click', function( e )
            {
                // we care only of row clicks
                if ( e.target.nodeName.toLowerCase() !== 'td' )
                {
                    return;
                }

                // get the unique id for this test case
                var id = e.target.parentNode.id.replace( /^_testcase_/, '' );
                doLoad( id );
            } );

            getPriorTable = function()
            {
                return table;
            };

            return getPriorTable();
        };


        function loadPrior()
        {
            // first, clear out any existing results
            getPriorTable().clear();

            // load prior data from server
            var response = getXhrJsonSync( 'GET', prior_url ),
                table    = getPriorTable(),
                results  = response.results;

            // store the current set of test cases
            curset = results;

            // add test test case to the table
            for ( testcase in results )
            {
                var result = results[ testcase ];

                table.addRow(
                    result.id,
                    result.looksgood,
                    result.waiting,
                    result.date,

                    // comment
                    ( result.comment.replace( /\n/g, '<br />' )
                        || '<em>(no comment)</em>'
                    ),

                    // username (from hostname)
                    getUserFromHostname( result.hostname ),

                    // premium
                    ( '$' + ( result.premium || 0.00 ) ),

                    // expected premium
                    ( ( result.expected )
                        ? '$' + result.expected
                        : ( result.looksgood )
                            ? '$' + result.premium
                            : '-'
                    )
                );
            }

            table.setCount( results.length );
        }


        function getTestCaseData( id )
        {
            return getXhrJsonSync( 'GET', prior_url + '&id=' + id );
        }


        function getQuoteData( id, qdata_host )
        {
            try
            {
                return getXhrJsonSync(
                    'GET', prior_url + '&host=' + qdata_host + '&qid=' + id
                );
            }
            catch ( e )
            {
                return { error: 'Invalid response from server.' };
            }
        }


        function showRatingResultPage()
        {
            dom.location.hash = 'test-data';
        }


        function loadQuote( qid, host, bucket_override )
        {
            var data = getQuoteData( qid, host );
            if ( data.error !== 'OK' )
            {
                alert( data.error );
                return;
            }

            rater.fromMap( data.results.bucket, function( data )
            {
                bucket = data;
                emptyBucket();

                if ( bucket_override )
                {
                    overrideBucket( bucket_override );
                }

                showRatingResultPage();
                rate( bucket );
            } );
        }


        function loadPriorTestCase( id )
        {
            var casedata = getTestCaseData( id );

            if ( casedata.status !== 200 )
            {
                alert( 'Could not load test case.\n\n' + casedata.error );
            }

            var data = casedata.results;

            // display the message so that they know what they're looking at
            exports.setPriorMessage(
                data.hostname, data.comment, data.looksgood, id
            );

            // overwrite the bucket
            bucket = data.bucket;
            emptyBucket();

            // set this test case so that our next save overwrites it
            setTestCase( id, data.result );

            // make it obvious to the user that the data has been loaded
            clearSummaryPremium();
            showEntryForm();

            // prefill the comment and expected data on the submission form,
            // leaving room at the top for additional comments
            document.getElementById( 'final-comments' )
                .innerHTML = (
                    "\n\n\n" +
                    getPrevSubmitCommentText(
                        data.hostname,
                        data.comment
                    )
                );
            document.getElementById( 'final-expected' ).value = data.expected;

            // let the browser catch up and then perform rating
            setTimeout( function()
            {
                // switch to test data and rate
                document.location.hash = '#test-data';
                rate( bucket );
            }, 0 );
        }


        function getPrevSubmitCommentText( hostname, comment )
        {
            return "Previously submitted by " +
                getUserFromHostname( hostname ) + ": " + comment;
        }


        function retestAll( callback )
        {
            var queue   = [],
                skipped = 0,

                // regression test results, which may or may not be submitted to
                // the server
                history = {};

            // queue each of the test cases
            for ( var testcase in curset )
            {
                queue.push( curset[ testcase ] );
            }

            var count = failures = changed = 0,
                start = ( new Date() ).getTime();

            var run = function()
            {
                // do not pop(); we want to do them in order so it doesn't look
                // too odd to the user
                var test = queue.shift();

                // all done
                if ( !( test ) )
                {
                    var time = ( new Date() ).getTime() - start;

                    var msg = (
                        'Test complete. Re-ran ' + count + ' test(s) with ' +
                        failures + ' failure(s) in ' + ( time / 1000 ) + 's.' +
                        "\n\n" +

                        ( ( skipped )
                            ? skipped + " test(s) premium checks " +
                                "were skipped because they " +
                                "have no expected premium; please aid in the " +
                                "automated testing of these by selecting " +
                                "them and entering an expected premium when " +
                                "re-submitting it (by clicking Incorrect). " +
                                "These skipped tests are still noted if " +
                                "their premiums changed (in italics), but " +
                                "their success statuses are left untouched."
                            : ''
                        ) +

                        ( ( failures === 0 )
                            ? ( !skipped )
                                ? "\n\nYou should feel pretty sweet right now."
                                : ''
                            : "\n\nSomeone's got some splainin' to do."
                        ) +

                        ( ( !changed ) ? "\n\nNo test cases have changed." :
                        "\n\n" + changed + " case(s) changed." +
                        "\n\nWould you like the results of this regression " +
                        "to be recorded? This will cause the status of each " +
                        "test case to be updated as shown. If unsure, click " +
                        "'Cancel'."
                        )
                    );

                    // if we have changes, show a box asking if the changes
                    // should be uploaded to the server; otheriwse, just alert
                    // (which will return undefined and cast to false)
                    var submit = !!( changed && confirm || alert )
                        .call( window, msg );

                    if ( submit )
                    {
                        saveRegression( history );
                    }

                    callback && callback( count, failures, time );

                    return;
                }

                var table = getPriorTable();
                table.mark( test.id, 'testing' );

                setTimeout( function()
                {
                    var testdata = getTestCaseData( test.id ).results;

                    try
                    {
                        // rate, but do not update the screen
                        rate( testdata.bucket, false, true );
                    }
                    catch ( e )
                    {
                        console.log( e );
                        table.mark( test.id, 'skip' );

                        // abort! abort!
                        //alert( 'An error occurred. Aborting.' );
                        run();
                        return;
                    }

                    // determine what premium we're expecting (default to
                    // existing premium)
                    var expected = testdata.expected || testdata.result.premium,
                        skipme   = !( testdata.looksgood || testdata.expected );

                    var correct = (
                        rate_result.premium
                        && ( rate_result.premium == expected )
                    );

                    // add to changed count if the status changed
                    var has_changed = ( testdata.looksgood !== correct );
                    changed += ( +has_changed && !skipme );

                    skipped += +skipme;

                    // only add to the total count if the premium was actually
                    // compared
                    if ( !skipme )
                    {
                        count++;

                        if ( !( correct ) )
                        {
                            failures++;
                        }

                        // store in case the user decides to save to the server
                        if ( has_changed )
                        {
                            history[ test.id ] = {
                                looksgood: correct,
                                bucket:    testdata.bucket,
                                result:    rate_result,
                                expected:  expected,
                                comment:   testdata.comment,
                                hostname:  testdata.hostname,
                                previous:  testdata.result,
                            };

                            // show the comment that would be saved to the
                            // server, should they choose to do so
                            table.changeComment(
                                test.id,
                                genRegressionComment( history[ test.id ] )
                            );
                        }
                    }

                    // update table
                    table.changePremium( test.id, rate_result.premium );
                    table.mark(
                        test.id,
                        (
                            ( ( correct )
                                ? 'good'
                                : 'bad'
                            ) +
                            ( ( has_changed )
                                ? ' changed'
                                : ''
                            ) +
                            ( ( rate_result.premium !== testdata.result.premium )
                                ? ' premchanged'
                                : ''
                            ) +
                            ( ( skipme )
                                ? ' skipped'
                                : ''
                            )
                        )
                    );

                    // continue
                    run();
                }, 0 );
            }

            // run 'em one by one
            setTimeout( run, 0 );
        }


        function genRegressionComment( item )
        {
            return "[Regression Test: " +
                ( ( item.looksgood ) ? "Pass" : "Fail" ) +
                "] Expected $" + item.expected + "; calculated $" +
                item.result.premium + "; previously $" +
                item.previous.premium + "\n\n" +
                getPrevSubmitCommentText( item.hostname, item.comment );
        }


        function saveRegression( history )
        {
            for ( var id in history )
            {
                var item = history[ id ];

                // generate comment
                var comment = genRegressionComment( item );

                submitQuote(
                    item.bucket,
                    item.result,
                    comment,
                    item.looksgood,
                    ( item.waiting || false ),
                    item.expected,
                    id,
                    function() {}
                );
            }
        }


        exports.initHtml = function()
        {
            getLoadDialog();
        }


        exports.showLoad = function()
        {
            removeEntryFocus();

            getLoadDialog().className += ' show';
            loadPrior();
        }


        exports.hideLoad = function()
        {
            var dialog = getLoadDialog();
            dialog.className = dialog.className.replace( /\bshow\b/g, '' );
        }


        exports.setPriorMessage = function( host, message, good, id )
        {
            var container = document.getElementById( 'prior-message' );

            if ( !container )
            {
                return;
            }

            const direct_link = ( id )
                ? '<br /><br /><a href="#prior/' + id + '">[Direct Link]</a>'
                : '';


            container.style.display = ( message ) ? 'inline-block' : 'none';
            container.className     = ( good ) ? 'good' : 'bad';
            container.innerHTML     = (
                ( host ? '<b>' + getUserFromHostname( host ) + ':</b> ' : '' ) +
                message
                    .replace( /^\n+|\n+$/g, '' )
                    .replace( /  /g, ' &nbsp;' )
                    .replace( /\t/g, '&nbsp;&nbsp;&nbsp;&nbsp;' )
                    .replace( /\n/g, '<br />' )
                    .replace(
                        /(Previously submitted by [^:]+:)/g,
                        '<b>$1</b>'
                    )
                    + direct_link
            );
        };

        exports.loadQuote         = loadQuote;
        exports.loadPriorTestCase = loadPriorTestCase;

        return exports;
    } )( document );


    function begin()
    {
        // initialize prior div
        Prior.initHtml();

        // allow linking to test cases
        var pmatch;
        if ( pmatch = document.location.href.match( /#prior(?:\/(.*))?$/ ) )
        {
            var id = pmatch[ 1 ];

            if ( !( id ) )
            {
                // no id given; let them choose
                Prior.showLoad();
            }
            else
            {
                // we were given an id; load it!
                console.log( 'Loading ' + id + '...' );
                Prior.loadPriorTestCase( id );
            }
        }

        // allow settings params from the URL (very basic parsing; barely used); we
        // use a colon rather than ? because ? is not included in the location
        // object
        var pdata;
        var bucket_override = [];
        if ( pdata = document.location.hash.match( /:(.*)$/ ) )
        {
            try
            {
                // params delimited by &
                var params = pdata[1].split( '&' );
                for ( var param in params )
                {
                    // values delimited from the name by =
                    var valdata = params[ param ].split( '=' ),
                        val     = JSON.parse( valdata[ 1 ] );

                    bucket_override[ valdata[ 0 ] ] = val;
                    bucket[ valdata[ 0 ] ]          = val;

                    console.log( 'Bucket override: ' + valdata[ 0 ] + '=' + val );
                }

                overrideBucket( bucket_override );
            }
            catch ( e )
            {
                // probably not the type of data we're looking for; just ignore
            }
        }

        // allow loading of quote ids
        var mdata;
        if ( mdata = document.location.hash.match( /#load\/([a-z]+)\/([0-9]+)/ ) )
        {
            var host = mdata[1],
                id   = mdata[2];

            // load the quote
            Prior.loadQuote( id, host, bucket_override );
        }
    }


    var vpt     = [ 0, 0, 0 ],
        vpt_cur = 0;
    function voiPainterAdd( tr, value )
    {
        var c = 'sel' + vpt_cur;
        tr.classList.toggle( c );

        vpt[ vpt_cur ] += ( value * ( tr.classList.contains( c ) ? 1 : -1 ) );
        vpt[ vpt_cur ] = +( vpt[ vpt_cur ].toFixed( 6 ) );

        showVoiPainter( vpt[ vpt_cur ] );
    }


    var vp_element = null,
        vpt_dest   = [];
    function showVoiPainter( val )
    {
        if ( !vp_element )
        {
            vp_element = document.createElement( 'div' );
            vp_element.id = 'voi-painter';

            for ( var i in vpt )
            {
                vpt_dest[ i ] = document.createElement( 'div' );
                vpt_dest[ i ].classList.add( 'sel' + i );
                vpt_dest[ i ].innerHTML = '0';
                vp_element.appendChild( vpt_dest[ i ] );

                ( function( i )
                {
                    vpt_dest[ i ].addEventListener( 'click', function()
                    {
                        vpt_cur = i;
                    } );
                } )( i );
            }

            document.getElementById( 'test-data' ).appendChild( vp_element );
        }

        vpt_dest[ vpt_cur ] .innerHTML = val;
    }


    return {
        updateSummaryDebug: updateSummaryDebug,

        onRate: function( callback )
        {
            rate_callback = callback;
        },

        begin: begin,
        Prior: Prior,
    };
} )();
