unit SBD.AP24.httpd;

{ This unit is a pascal wrapper for the Apache interface header files. Those  }
{ files were released under the following copyright:                          }
{                                                                             }
(* ====================================================================
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 2000-2002 The Apache Software Foundation.  All rights
 * reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 *
 * 3. The end-user documentation included with the redistribution,
 *    if any, must include the following acknowledgment:
 *       "This product includes software developed by the
 *        Apache Software Foundation (http://www.apache.org/)."
 *    Alternately, this acknowledgment may appear in the software itself,
 *    if and wherever such third-party acknowledgments normally appear.
 *
 * 4. The names "Apache" and "Apache Software Foundation" must
 *    not be used to endorse or promote products derived from this
 *    software without prior written permission. For written
 *    permission, please contact apache@apache.org.
 *
 * 5. Products derived from this software may not be called "Apache",
 *    nor may "Apache" appear in their name, without prior written
 *    permission of the Apache Software Foundation.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESSED OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED.  IN NO EVENT SHALL THE APACHE SOFTWARE FOUNDATION OR
 * ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
 * OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 * ====================================================================
 *
 * This software consists of voluntary contributions made by many
 * individuals on behalf of the Apache Software Foundation.  For more
 * information on the Apache Software Foundation, please see
 * <http://www.apache.org/>.
 *
 * Portions of this software are based upon public domain software
 * originally written at the National Center for Supercomputing Applications,
 * University of Illinois, Urbana-Champaign.
 *)

interface
{$ifdef MSWINDOWS}
  uses WinSock, Windows;
{$endif}
{$ifdef MSWINDOWS}
  {$ALIGN 8}
{$endif}

{$define APR_HAS_THREADS}

const
  MODULE_MAGIC_COOKIE = $41503234; // "AP24"
  MODULE_MAGIC_NUMBER_MAJOR = 20120211;
  MODULE_MAGIC_NUMBER_MINOR = 27;

  APR_FINFO_LINK   = $00000001; // Stat the link not the file itself if it is a link
  APR_FINFO_MTIME  = $00000010; // Modification Time
  APR_FINFO_CTIME  = $00000020; // Creation or inode-changed time
  APR_FINFO_ATIME  = $00000040; // Access Time
  APR_FINFO_SIZE   = $00000100; // Size of the file
  APR_FINFO_CSIZE  = $00000200; // Storage size consumed by the file
  APR_FINFO_DEV    = $00001000; // Device
  APR_FINFO_INODE  = $00002000; // Inode
  APR_FINFO_NLINK  = $00004000; // Number of links
  APR_FINFO_TYPE   = $00008000; // Type
  APR_FINFO_USER   = $00010000; //  User
  APR_FINFO_GROUP  = $00020000; // Group
  APR_FINFO_UPROT  = $00100000; // User protection bits
  APR_FINFO_GPROT  = $00200000; // Group protection bits
  APR_FINFO_WPROT  = $00400000; // World protection bits
  APR_FINFO_ICASE  = $01000000; // if dev is case insensitive
  APR_FINFO_NAME   = $02000000; // ->name in proper case

  APR_FINFO_MIN    = $00008170; // type, mtime, ctime, atime, size
  APR_FINFO_IDENT  = $00003000; // dev and inode
  APR_FINFO_OWNER  = $00030000; // user and group
  APR_FINFO_PROT   = $00700000; // all protections
  APR_FINFO_NORM   = $0073b170; // an atomic unix apr_stat()
  APR_FINFO_DIRENT = $02000000; // an atomic unix apr_dir_read()

// Possible values for request_rec.read_body (set by handling module):
  REQUEST_NO_BODY         = 0; // Send 413 error if message has any body
  REQUEST_CHUNKED_ERROR   = 1; // Send 411 error if body without Content-Length
  REQUEST_CHUNKED_DECHUNK = 2;// f chunked, remove the chunks for me.

  // Hook orderings
  APR_HOOK_REALLY_FIRST	= -10; // run this hook first, before ANYTHING
  APR_HOOK_FIRST        =   0; // run this hook first
  APR_HOOK_MIDDLE		    =  10; // run this hook somewhere
  APR_HOOK_LAST		      =  20; // run this hook after every other hook which is defined
  APR_HOOK_REALLY_LAST	=  30; // run this hook last, after EVERYTHING

  HTTP_OK = 200;
  AP_OK = 0;
  AP_DECLINED = -1;
  HTTP_MOVED_TEMPORARILY = 302;
{$ifdef MSWINDOWS}
  libhttpd = 'libhttpd.dll';
  libapr_1 = 'libapr-1.dll';
{$endif}

// @defgroup ConfigDirectives Allowed locations for configuration directives.
// The allowed locations for a configuration directive are the union of
// those indicated by each set bit in the req_override mask.
  OR_NONE      =   0;       // *.conf is not available anywhere in this override
  OR_LIMIT     =   1;       // *.conf inside <Directory> or <Location>
                            //    and .htaccess when AllowOverride Limit
  OR_OPTIONS   =   2;       // *.conf anywhere
                            //    and .htaccess when AllowOverride Options
  OR_FILEINFO  =   4;       // *.conf anywhere
                            //    and .htaccess when AllowOverride FileInfo
  OR_AUTHCFG   =   8;       // *.conf inside <Directory> or <Location>
                            //    and .htaccess when AllowOverride AuthConfig
  OR_INDEXES   =  16;       // *.conf anywhere
                            //    and .htaccess when AllowOverride Indexes
  OR_UNSET     =  32;       // bit to indicate that AllowOverride has not been set
  ACCESS_CONF  =  64;       // *.conf inside <Directory> or <Location>
  RSRC_CONF    = 128;       // *.conf outside <Directory> or <Location>
  EXEC_ON_READ = 256;       // force directive to execute a command
                // which would modify the configuration (like including another
                // file, or IFModule
  OR_ALL = OR_LIMIT or OR_OPTIONS or OR_FILEINFO or OR_AUTHCFG or OR_INDEXES;

type
apr_pool_t = record end;
Papr_pool_t = ^apr_pool_t;
Pserver_rec = ^server_rec;

apr_int64_t = int64;
apr_uint64_t = uint64;
apr_time_t = apr_int64_t;
apr_ino_t = apr_uint64_t;
apr_uint32_t = cardinal;

{$ifdef MSWINDOWS}
  apr_uid_t = PSID;
{$else}
  apr_uid_t = uid_t;
{$endif}

// Structure for determining group ownership.
{$ifdef MSWINDOWS}
  apr_gid_t = PSID;
{$else}
  apr_gid_t = gid_t;
{$endif}


// Structure for determining the device the file is on.
{$ifdef MSWINDOWS}  // #if (defined WIN32) || (defined NETWARE)
  apr_dev_t = apr_uint32_t;
{$else}
  apr_dev_t dev_t;
{$endif}

Pprocess_rec = ^process_rec;
process_rec = record
    // Global pool. Cleared upon normal exit
    pool: Papr_pool_t;

    // Configuration pool. Cleared upon restart
    pconf: Papr_pool_t;

    // The program name used to execute the program
    short_name: PAnsichar;

    // The command line arguments
    argv: PPAnsiChar;

    // Number of command line arguments passed to the program
    argc: integer;
  end;

Papr_file_t = ^apr_file_t;
apr_file_t = record end;

ap_logconf = record
    // The per-module log levels
    module_levels: PAnsiChar;

    // The log level for this server
    level: integer;
  end;
Pap_logconf = ^ap_logconf;

Pap_conf_vector_t = ^ap_conf_vector_t;
ap_conf_vector_t = record end;

apr_port_t = word;
apr_int32_t = integer;
apr_socklen_t = integer;
apr_fileperms_t = apr_int32_t;

  sockaddr_in = record
    case Integer of
      0: (sin_family: u_short;
          sin_port: u_short;
          sin_addr: TInAddr;
          sin_zero: array[0..7] of AnsiChar);
      1: (sa_family: u_short;
          sa_data: array[0..13] of AnsiChar)
  end;

in6_addr = record
    case Integer of
      0: (s6_addr: array[0..15] of u_char);
      1: (word: array[0..7] of u_short);
  end;

sockaddr_in6 = record
    sin6_family: smallint;
    sin6_port: u_short;
    sin6_flowinfo: u_long;
    sin6_addr: in6_addr;
    sin6_scope_id: u_long;
  end;
sockaddr_storage = record
    ss_family: smallint;
    __ss_pad1: array[0..5] of AnsiChar;
    __ss_align: Int64;
    __ss_pad2: array[0..111] of AnsiChar;
  end;

Papr_sockaddr_t = ^apr_sockaddr_t;
apr_sockaddr_t = record
    pool: Papr_pool_t;   // The pool to use...
    hostname: PAnsichar;    // The hostname
    servname: PAnsichar;   // Either a string of the port number or the service name for the port
    port: apr_port_t;  // The numeric port
    family: apr_int32_t; // The family
    salen: apr_socklen_t;  // How big is the sockaddr we're using?
    ipaddr_len: integer;  // How big is the ip address structure we're using?
    addr_str_len: integer;  // How big should the address buffer be?  16 for v4 or 46 for v6
    ipaddr_ptr: pointer; // This points to the IP address structure within the appropriate sockaddr structure
    // If multiple addresses were found by apr_sockaddr_info_get(), this
    //  points to a representation of the next address.
    next: Papr_sockaddr_t;
    sa: record  // Union of either IPv4 or IPv6 sockaddr.
      case Integer of
        4: (sin: sockaddr_in); // Pv4 sockaddr structure
        6: (sin6: sockaddr_in6);  // IPv6 sockaddr structure
        // Placeholder to ensure that the size of this union is not
        //  dependent on whether APR_HAVE_IPV6 is defined.
        0: (sas: sockaddr_storage);
      end
  end;

Pserver_addr_rec = ^server_addr_rec;
server_addr_rec = record
    next: Pserver_addr_rec;
    virthost: PAnsichar; // The name given in "<VirtualHost>"
    host_addr: Papr_sockaddr_t; // The bound address, for this server
    host_port: apr_port_t; // The bound port, for this server
  end;

apr_interval_time_t = int64;
Papr_array_header_t = ^apr_array_header_t;
apr_array_header_t = record
    pool: Papr_pool_t;  // The pool the array is allocated out of
    elt_size: integer; // The amount of memory allocated for each element of the array
    nelts: integer; // The number of active elements in the array
    nalloc: integer; // The number of elements allocated in the array
    elts: PAnsichar;  // The elements in the array
  end;

server_rec = record
    // The process this server is running in
    process: Pprocess_rec;

    // The next server in the list
    next: Pserver_rec;

    // The name of the error log
    error_fname: PAnsichar;

    // A file descriptor that references the error log
    error_log: Papr_file_t;

    // The log level configuration
    log: ap_logconf;

    module_config: Pap_conf_vector_t;

    // MIME type info, etc., before we start checking per-directory info
    lookup_defaults: Pap_conf_vector_t;

    // The name of the server
    defn_name: PAnsiChar;

    // The line of the config file that the server was defined on
    defn_line_number: cardinal;

    // true if this is the virtual server
    is_virtual: Ansichar;

    // for redirects, etc.
    port: apr_port_t;

    // The server request scheme for redirect responses
    server_scheme: PAnsichar;

    // The admin's contact information
    server_admin: PAnsichar;

    // The server hostname
    server_hostname: PAnsichar;

    addrs: Pserver_addr_rec;

    // Timeout, as an apr interval, before we give up
    timeout: apr_interval_time_t;

    // The apr interval we will wait for another request
    keep_alive_timeout: apr_interval_time_t;

    // Maximum requests per connection
    keep_alive_max: integer;

    // Use persistent connections?
    keep_alive: integer;

    // Normal names for ServerAlias servers
    names: Papr_array_header_t;

    // Wildcarded names for ServerAlias servers
    wild_names: Papr_array_header_t;

    // Pathname for ServerPath
    path: PAnsichar;

    // Length of path
    pathlen: integer;

    // limit on size of the HTTP request line
    limit_req_line: integer;

    // limit on size of any request header field
    limit_req_fieldsize: integer;

    // limit on number of request header fields
    limit_req_fields: integer;

    // Opaque storage location
    context: pointer;
  end;


Papr_table_t = ^apr_table_t;
apr_table_t = record end;

Pap_filter_t = ^ap_filter_t;

Tis_metadata = (APR_BUCKET_DATA,      // This bucket type represents actual data to send to the client.
                APR_BUCKET_METADATA); // This bucket type represents metadata.

apr_read_type_e = ( // Determines how a bucket or brigade should be read
    APR_BLOCK_READ,   // block until data becomes available
    APR_NONBLOCK_READ // return immediately if no data is available
  );

apr_status_t = integer;
apr_size_t = integer;
apr_off_t = int64;

Papr_bucket = ^apr_bucket;
Papr_bucket_type_t = ^apr_bucket_type_t;

Papr_bucket_alloc_t = ^apr_bucket_alloc_t;
apr_bucket_alloc_t = record end;

apr_bucket = record
    link: record	// Links to the rest of the brigade
            next: Papr_bucket;
            prev: Papr_bucket;
          end;

    type1: Papr_bucket_type_t;

    // The length of the data in the bucket.  This could have been implemented
    //  with a function, but this is an optimization, because the most
    //  common thing to do will be to get the length.  If the length is unknown,
    //  the value of this field will be (apr_size_t)(-1).
    length: apr_size_t;

    // The start of the data in the bucket relative to the private base
    //  pointer.  The vast majority of bucket types allow a fixed block of
    //  data to be referenced by multiple buckets, each bucket pointing to
    //  a different segment of the data.  That segment starts at base+start
    //  and ends at base+start+length.
    //  If the length == (apr_size_t)(-1), then start == -1.
    start: apr_off_t;
    data: pointer; // type-dependent data hangs off this pointer

    // Pointer to function used to free the bucket. This function should
    // always be defined and it should be consistent with the memory
    // function used to allocate the bucket. For example, if malloc() is
    // used to allocate the bucket, this pointer should point to free().
    // @param e Pointer to the bucket being freed
    free: procedure( e: pointer); // void (*free)(void *e);
    list: Papr_bucket_alloc_t;  // The freelist from which this bucket was allocated
  end;

apr_bucket_type_t = record
    name: PAnsichar; // The name of the bucket type
    num_func: integer; // The number of functions this bucket understands.  Can not be less than five.

    // Whether the bucket contains metadata (ie, information that
    // describes the regular contents of the brigade).  The metadata
    // is not returned by apr_bucket_read() and is not indicated by
    // the ->length of the apr_bucket itself.  In other words, an
    // empty bucket is safe to arbitrarily remove if and only if it
    // contains no metadata.  In this sense, "data" is just raw bytes
    // that are the "content" of the brigade and "metadata" describes
    // that data but is not a proper part of it.
    is_metadata: Tis_metadata;

    // Free the private data and any resources used by the bucket (if they
    //  aren't shared with another bucket).  This function is required to be
    //  implemented for all bucket types, though it might be a no-op on some
    //  of them (namely ones that never allocate any private data structures).
    // @param data The private data pointer from the bucket to be destroyed
    destroy: procedure( data: pointer); cdecl; // void (*destroy)(void *data);

    // Read the data from the bucket. This is required to be implemented
    //  for all bucket types.
    // @param b The bucket to read from
    // @param str A place to store the data read.  Allocation should only be
    //            done if absolutely necessary.
    // @param len The amount of data read.
    // @param block Should this read function block if there is more data that
    //              cannot be read immediately.
    read: function( b: Papr_bucket; var str: PAnsichar; var len: integer; block: apr_read_type_e): apr_status_t; cdecl;
    // apr_status_t (*read)(apr_bucket *b, const char **str, apr_size_t *len,
    //                      apr_read_type_e block);

    // Make it possible to set aside the data for at least as long as the
    //  given pool. Buckets containing data that could potentially die before
    //  this pool (e.g. the data resides on the stack, in a child pool of
    //  the given pool, or in a disjoint pool) must somehow copy, shift, or
    //  transform the data to have the proper lifetime.
    // @param e The bucket to convert
    // @remark Some bucket types contain data that will always outlive the
    //         bucket itself. For example no data (EOS and FLUSH), or the data
    //         resides in global, constant memory (IMMORTAL), or the data is on
    //      the heap (HEAP). For these buckets, apr_bucket_setaside_noop can
    //      be used.
    // apr_status_t (*setaside)(apr_bucket *e, apr_pool_t *pool);
    setaside: function( e: Papr_bucket; pool: Papr_pool_t): apr_status_t; cdecl;

    // Split one bucket in two at the specified position by duplicating
    //  the bucket structure (not the data) and modifying any necessary
    //  start/end/offset information.  If it's not possible to do this
    //  for the bucket type (perhaps the length of the data is indeterminate,
    //  as with pipe and socket buckets), then APR_ENOTIMPL is returned.
    // @param e The bucket to split
    // @param point The offset of the first byte in the new bucket
    // apr_status_t (*split)(apr_bucket *e, apr_size_t point);
    split: function( var e: apr_bucket; point: apr_size_t): apr_status_t; cdecl;

    // Copy the bucket structure (not the data), assuming that this is
    //  possible for the bucket type. If it's not, APR_ENOTIMPL is returned.
    // @param e The bucket to copy
    // @param c Returns a pointer to the new bucket
    //apr_status_t (*copy)(apr_bucket *e, apr_bucket **c);
    copy: function( var e: apr_bucket; var c: Papr_bucket): apr_status_t; cdecl;
  end;


Papr_bucket_brigade = ^apr_bucket_brigade;
/// A list of buckets
apr_bucket_brigade = record
    // The pool to associate the brigade with.  The data is not allocated out
    //  of the pool, but a cleanup is registered with this pool.  If the
    //  brigade is destroyed by some mechanism other than pool destruction,
    //  the destroying function is responsible for killing the cleanup.
    p: Papr_pool_t;

    // The buckets in the brigade are on this list. */
    // The apr_bucket_list structure doesn't actually need a name tag
    // because it has no existence independent of struct apr_bucket_brigade;
    // the ring macros are designed so that you can leave the name tag
    // argument empty in this situation but apparently the Windows compiler
    // doesn't like that.
    list: record // apr_bucket_list
            next: Papr_bucket;
            prev: Papr_bucket;
          end;
    bucket_alloc: Papr_bucket_alloc_t; // The freelist from which this bucket was allocated
  end;

ap_input_mode_t = (
    AP_MODE_READBYTES, // The filter should return at most readbytes data.

    // The filter should return at most one line of CRLF data.
    //  (If a potential line is too long or no CRLF is found, the
    //   filter may return partial data).
    AP_MODE_GETLINE,
    AP_MODE_EATCRLF, // The filter should implicitly eat any CRLF pairs that it sees.

    // The filter read should be treated as speculative and any returned
    //  data should be stored for later retrieval in another mode. */
    AP_MODE_SPECULATIVE,

    // The filter read should be exhaustive and read until it can not
    //  read any more.
    //  Use this mode with extreme caution.
    AP_MODE_EXHAUSTIVE,

    // The filter should initialize the connection if needed,
    //  NNTP or FTP over SSL for example.
    AP_MODE_INIT);

ap_out_filter_func = function( f: Pap_filter_t; b: Papr_bucket_brigade): apr_status_t; cdecl;
ap_in_filter_func  = function( f: Pap_filter_t; b: Papr_bucket_brigade; mode: ap_input_mode_t; block: apr_read_type_e; readbytes: apr_off_t): apr_status_t; cdecl;

Pap_filter_func = ^ap_filter_func;
ap_filter_func = record
    case integer of
      0: (out_func: ap_out_filter_func);
      1: (in_func: ap_in_filter_func);
  end;

Pap_filter_rec_t = ^ap_filter_rec_t;
Pconn_rec = ^conn_rec;
Prequest_rec = ^request_rec;

ap_filter_t = record
    // The internal representation of this filter.  This includes
    //  the filter's name, type, and the actual function pointer.
    frec: Pap_filter_rec_t;
    ctx: pointer; // A place to store any data associated with the current filter
    next: Pap_filter_t; // The next filter in the chain

    // The request_rec associated with the current filter.  If a sub-request
    //  adds filters, then the sub-request is the request associated with the
    //  filter.
    r: Prequest_rec;

    // The conn_rec associated with the current filter.  This is analogous
    //  to the request_rec, except that it is used for connection filters.
    c: Pconn_rec;
  end;

// typedef int (*ap_init_filter_func)(ap_filter_t *f);
ap_init_filter_func = function( var f: ap_filter_t): integer; cdecl;

Pap_filter_provider_t = ^ap_filter_provider_t;
ap_filter_provider_t = record end;

// Filters have different types/classifications. These are used to group
// and sort the filters to properly sequence their operation.
// The types have a particular sort order, which allows us to insert them
// into the filter chain in a determistic order. Within a particular grouping,
// the ordering is equivalent to the order of calls to ap_add_*_filter().
ap_filter_type = (
    // These filters are used to alter the content that is passed through
    //  them. Examples are SSI or PHP. */
    AP_FTYPE_RESOURCE     = 10,

    // These filters are used to alter the content as a whole, but after all
    //  AP_FTYPE_RESOURCE filters are executed.  These filters should not
    //  change the content-type.  An example is deflate.  */
    AP_FTYPE_CONTENT_SET  = 20,

    // These filters are used to handle the protocol between server and
    //  client.  Examples are HTTP and POP. */
    AP_FTYPE_PROTOCOL     = 30,

    //  These filters implement transport encodings (e.g., chunking). */
    AP_FTYPE_TRANSCODE    = 40,

    // These filters will alter the content, but in ways that are
    //  more strongly associated with the connection.  Examples are
    //  splitting an HTTP connection into multiple requests and
    //  buffering HTTP responses across multiple requests.
    //  It is important to note that these types of filters are not
    //  allowed in a sub-request. A sub-request's output can certainly
    //  be filtered by ::AP_FTYPE_RESOURCE filters, but all of the "final
    //  processing" is determined by the main request.
    AP_FTYPE_CONNECTION  = 50,

    // These filters don't alter the content.  They are responsible for
    //  sending/receiving data to/from the client. */
    AP_FTYPE_NETWORK     = 60);

ap_filter_rec_t = record
    name: PAnsichar; // The registered name for this filter
    filter_func: ap_filter_func; // The function to call when this filter is invoked.

    // The function to call directly before the handlers are invoked
    // for a request.  The init function is called once directly
    // before running the handlers for a request or subrequest.  The
    // init function is never called for a connection filter (with
    // ftype >= AP_FTYPE_CONNECTION).  Any use of this function for
    // filters for protocols other than HTTP is specified by the
    // module supported that protocol.
    filter_init_func: ap_init_filter_func;
    next: Pap_filter_rec_t; // The next filter_rec in the list
    providers: Pap_filter_provider_t; // Providers for this filter

    // The type of filter, either AP_FTYPE_CONTENT or AP_FTYPE_CONNECTION.
    // An AP_FTYPE_CONTENT filter modifies the data based on information
    // found in the content.  An AP_FTYPE_CONNECTION filter modifies the
    // data based on the type of connection.
    ftype: ap_filter_type;
    debug: integer; // Trace level for this filter
    proto_flags: cardinal;  // Protocol flags for this filter
  end;


Pap_method_list_t = ^ap_method_list_t;
// @struct ap_method_list_t
// @brief  Structure for handling HTTP methods.
// Methods known to the server are accessed via a bitmask shortcut;
// extension methods are handled by an array.
ap_method_list_t = record
    method_mask: apr_int64_t; // The bitmask used for known methods
    method_list: Papr_array_header_t; // the array used for extension methods
  end;

// @brief This represents the result of calling htaccess; these are cached for
// each request.
Phtaccess_result = ^htaccess_result;
htaccess_result = record
    dir: PAnsichar; // the directory to which this applies
    override: integer; // the overrides allowed for the .htaccess file
    override_opts: integer; // the override options allowed for the .htaccess file
    override_list: Papr_table_t; // Table of allowed directives for override
    htaccess: Pap_conf_vector_t; // the configuration directives
    next: Phtaccess_result;  // the next one, or NULL if no more; N.B. never change this
  end;

Papr_thread_mutex_t = ^apr_thread_mutex_t;
apr_thread_mutex_t = record end; // opaque

Papr_uri_t = ^apr_uri_t;
apr_uri_t = record // A structure to encompass all of the fields in a uri
    scheme: PAnsichar; // scheme ("http"/"ftp"/...)
    hostinfo: PAnsichar; // combined [user[:password]\@]host[:port]
    user: PAnsichar; //user name, as in http://user:passwd\@host:port/
    password: PAnsichar; // password, as in http://user:passwd\@host:port/
    hostname: PAnsichar; // hostname from URI (or from Host: header)
    port_str: PAnsichar; // port string (integer representation is in "port")
    path: PAnsichar; // the request path (or NULL if only scheme://host was given)
    query: PAnsichar; // Everything after a '?' in the path, if present
    fragment: PAnsichar; // Trailing "#fragment" string, if present
    hostent: Phostent; // structure returned from gethostbyname()
    port: apr_port_t; // The port number, numeric, valid only if port_str != NULL
    is_initialized: cardinal; // has the structure been initialized  is_initialized:1
    dns_looked_up: cardinal; // has the DNS been looked up yet :1;
    dns_resolved: cardinal; // has the dns been resolved yet :1;
  end;

// Many applications use the type member to determine the
// existance of a file or initialization of the file info,
// so the APR_NOFILE value must be distinct from APR_UNKFILE.

// apr_filetype_e values for the filetype member of the
// apr_file_info_t structure
// @warning: Not all of the filetypes below can be determined.
// For example, a given platform might not correctly report
// a socket descriptor as APR_SOCK if that type isn't
// well-identified on that platform.  In such cases where
// a filetype exists but cannot be described by the recognized
// flags below, the filetype will be APR_UNKFILE.  If the
// filetype member is not determined, the type will be APR_NOFILE.

apr_filetype_e = (
    APR_NOFILE = 0,     // no file type determined
    APR_REG,            // a regular file
    APR_DIR,            // a directory
    APR_CHR,            // a character device
    APR_BLK,            // a block device
    APR_PIPE,           // a FIFO / pipe
    APR_LNK,            // a symbolic link
    APR_SOCK,           // a [unix domain] socket
    APR_UNKFILE = 127); // a file of some other unknown type

// The file information structure.  This is analogous to the POSIX
// stat structure.
Papr_finfo_t = ^apr_finfo_t;
apr_finfo_t = record
    pool: Papr_pool_t; // Allocates memory and closes lingering handles in the specified pool

    // The bitmask describing valid fields of this apr_finfo_t structure
    //  including all available 'wanted' fields and potentially more */
    valid: apr_int32_t;
    protection: apr_fileperms_t; // The access permissions of the file.  Mimics Unix access rights.
    // The type of file.  One of APR_REG, APR_DIR, APR_CHR, APR_BLK, APR_PIPE,
    // APR_LNK or APR_SOCK.  If the type is undetermined, the value is APR_NOFILE.
    // If the type cannot be determined, the value is APR_UNKFILE.
    filetype: apr_filetype_e;
    user: apr_uid_t; // The user id that owns the file
    group: apr_gid_t; // The group id that owns the file
    inode: apr_ino_t; // The inode of the file.
    device: apr_dev_t; // The id of the device the file is on.
    nlink: apr_int32_t; // The number of hard links to the file.
    size: apr_off_t;  // The size of the file
    csize: apr_off_t; // The storage size consumed by the file
    atime: apr_time_t; // The time the file was last accessed
    mtime: apr_time_t; // The time the file was last modified
    ctime: apr_time_t; // The time the file was created, or the inode was last changed
    fname: PAnsichar; // The pathname of the file (possibly unrooted)
    name: PAnsichar; // The file's name (no path) in filesystem case
    filehand: Papr_file_t; // The file's handle, if accessed (can be submitted to apr_duphandle)
  end;

request_rec = record // brief A structure that represents the current request
    pool: Papr_pool_t; // The pool associated with the request
    connection: Pconn_rec;  // The connection to the client
    server: Pserver_rec; // The virtual host for this request
    next: Prequest_rec; // Pointer to the redirected request if this is an external redirect
    prev: Prequest_rec; // Pointer to the previous request if this is an internal redirect

    // Pointer to the main request if this is a sub-request
    // (see http_request.h) */
    main: Prequest_rec;
    the_request: PAnsichar; // First line of request
    assbackwards: integer; // HTTP/0.9, "simple" request (e.g. GET /foo\n w/no headers)

    // A proxy request (calculated during post_read_request/translate_name)
    //  possible values PROXYREQ_NONE, PROXYREQ_PROXY, PROXYREQ_REVERSE,
    //                  PROXYREQ_RESPONSE
    proxyreq: integer;
    header_only: integer;  // HEAD request, as opposed to GET
    proto_num: integer; // Protocol version number of protocol; 1.1 = 1001
    protocol: PAnsichar; // Protocol string, as given to us, or HTTP/0.9
    hostname: PAnsichar; // Host, as set by full URI or Host:
    request_time: apr_time_t; // Time when the request started
    status_line: PAnsichar; // Status line, if set by script
    status: integer; // Status line

    // Request method, two ways; also, protocol, etc..  Outside of protocol.c,
    // look, but don't touch.

    method_number: integer; // M_GET, M_POST, etc.
    method: PAnsichar; // Request method (eg. GET, HEAD, POST, etc.)

//     *  'allowed' is a bitvector of the allowed methods.
//     *
//     *  A handler must ensure that the request method is one that
//     *  it is capable of handling.  Generally modules should DECLINE
//     *  any request methods they do not handle.  Prior to aborting the
//     *  handler like this the handler should set r->allowed to the list
//     *  of methods that it is willing to handle.  This bitvector is used
//     *  to construct the "Allow:" header required for OPTIONS requests,
//     *  and HTTP_METHOD_NOT_ALLOWED and HTTP_NOT_IMPLEMENTED status codes.
//     *
//     *  Since the default_handler deals with OPTIONS, all modules can
//     *  usually decline to deal with OPTIONS.  TRACE is always allowed,
//     *  modules don't need to set it explicitly.
//     *
//     *  Since the default_handler will always handle a GET, a
//     *  module which does *not* implement GET should probably return
//     *  HTTP_METHOD_NOT_ALLOWED.  Unfortunately this means that a Script GET
//     *  handler can't be installed by mod_actions.

    allowed: apr_int64_t;
    allowed_xmethods: Papr_array_header_t; // Array of extension methods
    allowed_methods: Pap_method_list_t; // List of allowed methods
    sent_bodyct: apr_off_t; // byte count in stream is for body
    bytes_sent: apr_off_t; // body byte count, for easy access
    mtime: apr_time_t; // Last modified time of the requested resource
    range: PAnsichar; // The Range: header
    clength: apr_off_t; // The "real" content length
    chunked: integer; // sending chunked transfer-coding

    // Method for reading the request body
    // (eg. REQUEST_CHUNKED_ERROR, REQUEST_NO_BODY,
    //  REQUEST_CHUNKED_DECHUNK, etc...) */
    read_body: integer;
    read_chunked: integer;  // reading chunked transfer-coding
    expecting_100: cardinal;  // is client waiting for a 100 response?
    kept_body: Papr_bucket_brigade; // The optional kept body of the request.

    // For ap_body_to_table(): parsed body */
    // XXX: ap_body_to_table has been removed. Remove body_table too or
    // XXX: keep it to reintroduce ap_body_to_table without major bump? */
    body_table: Papr_table_t;
    remaining: apr_off_t; // Remaining bytes left to read from the request body
    read_length: apr_off_t; // Number of bytes that have been read  from the request body

    // MIME header environments, in and out.  Also, an array containing
    // environment variables to be passed to subprocesses, so people can
    // write modules to add to that environment.
    //
    // The difference between headers_out and err_headers_out is that the
    // latter are printed even on error, and persist across internal redirects
    // (so the headers printed for ErrorDocument handlers will have them).
    //
    // The 'notes' apr_table_t is for notes from one module to another, with no
    // other set purpose in mind...

    headers_in: Papr_table_t; // MIME header environment from the request
    headers_out: Papr_table_t; // MIME header environment for the response

    // MIME header environment for the response, printed even on errors and
    // persist across internal redirects
    err_headers_out: Papr_table_t;
    subprocess_env: Papr_table_t; // Array of environment variables to be used for sub processes
    notes: Papr_table_t; // Notes from one module to another

    // content_type, handler, content_encoding, and all content_languages
    // MUST be lowercased strings.  They may be pointers to static strings;
    // they should not be modified in place.

    // The content-type for the current request */
    content_type: PAnsichar;   // Break these out --- we dispatch on 'em
    // The handler string that we use to call a handler function
    handler: PAnsichar;        // What we *really* dispatch on
    content_encoding: PAnsichar;  // How to encode the data
    content_languages: papr_array_header_t; // Array of strings representing the content languages
    vlist_validator: PAnsichar; // variant list validator (if negotiated)
    user: PAnsichar; // If an authentication check was made, this gets set to the user name.
    ap_auth_type: PAnsichar; // If an authentication check was made, this gets set to the auth type.

    // What object is being requested (either directly, or via include
    // or content-negotiation mapping).

    unparsed_uri: PAnsichar; // The URI without any parsing performed
    uri: PAnsichar;  // The path portion of the URI, or "/" if no path provided
    filename: PAnsichar;  // The filename on disk corresponding to this response

    // XXX: What does this mean? Please define "canonicalize" -aaron
    // The true filename, we canonicalize r->filename if these don't match
    canonical_filename: PAnsichar;

    path_info: PAnsichar; // The PATH_INFO extracted from this request
    args: PAnsichar; // The QUERY_ARGS extracted from this request

    // Flag for the handler to accept or reject path_info on
    // the current request.  All modules should respect the
    // AP_REQ_ACCEPT_PATH_INFO and AP_REQ_REJECT_PATH_INFO
    // values, while AP_REQ_DEFAULT_PATH_INFO indicates they
    // may follow existing conventions.  This is set to the
    // user's preference upon HOOK_VERY_FIRST of the fixups.
    used_path_info: integer;

    // A flag to determine if the eos bucket has been sent yet
    eos_sent: integer;

    // Various other config info which may change with .htaccess files
    // These are config vectors, with one void* pointer for each module
    // (the thing pointed to being the module's business).
    per_dir_config: Pap_conf_vector_t; // Options set in config files, etc.
    request_config: Pap_conf_vector_t; // Notes on *this* request

    // Optional request log level configuration. Will usually point
    //  to a server or per_dir config, i.e. must be copied before
    //  modifying */
    log: Pap_logconf;

    // Id to identify request in access and error log. Set when the first
    //  error log entry for this request is generated.
    log_id: PAnsichar;

    // A linked list of the .htaccess configuration directives
    // accessed by this request.
    // N.B. always add to the head of the list, _never_ to the end.
    // that way, a sub request's list can (temporarily) point to a parent's list
    htaccess: Phtaccess_result;

    output_filters: Pap_filter_t; // A list of output filters to be used for this request
    input_filters: Pap_filter_t; // A list of input filters to be used for this request

    // A list of protocol level output filters to be used for this
    //  request *
    proto_output_filters: Pap_filter_t;

    // A list of protocol level input filters to be used for this
    //  request
    proto_input_filters: Pap_filter_t;

    no_cache: integer; // This response can not be cached
    no_local_copy: integer; // There is no local copy of this response

    // Mutex protect callbacks registered with ap_mpm_register_timed_callback
    // from being run before the original handler finishes running
    invoke_mtx: Papr_thread_mutex_t;

    parsed_uri: apr_uri_t; // A struct containing the components of URI
    finfo: apr_finfo_t;  // finfo.protection (st_mode) set to zero if no such file

    // remote address information from conn_rec, can be overridden if
    // necessary by a module.
    // This is the address that originated the request.
    useragent_addr: Papr_sockaddr_t;
    useragent_ip: PAnsichar;
  end;


// Enumeration of connection states
// The two states CONN_STATE_LINGER_NORMAL and CONN_STATE_LINGER_SHORT may
// only be set by the MPM. Use CONN_STATE_LINGER outside of the MPM.
conn_state_e = (
    CONN_STATE_CHECK_REQUEST_LINE_READABLE,
    CONN_STATE_READ_REQUEST_LINE,
    CONN_STATE_HANDLER,
    CONN_STATE_WRITE_COMPLETION,
    CONN_STATE_SUSPENDED,
    CONN_STATE_LINGER,          // connection may be closed with lingering
    CONN_STATE_LINGER_NORMAL,   // MPM has started lingering close with normal timeout
    CONN_STATE_LINGER_SHORT);   // MPM has started lingering close with short timeout

conn_sense_e = (
    CONN_SENSE_DEFAULT,
    CONN_SENSE_WANT_READ,       // next event must be read
    CONN_SENSE_WANT_WRITE);     // next event must be write

// @brief A structure to contain connection state information
conn_state_t = record
    state: conn_state_e; // Current state of the connection
    sense: conn_sense_e; // Whether to read instead of write, or write instead of read
  end;
Pconn_state_t = ^conn_state_t;

ap_conn_keepalive_e = ( // @brief Enumeration of connection keepalive options
    AP_CONN_UNKNOWN,
    AP_CONN_CLOSE,
    AP_CONN_KEEPALIVE);

Papr_thread_t = ^apr_thread_t;
apr_thread_t = record end;

conn_rec = packed record
    // Pool associated with this connection
    pool: Papr_pool_t;

    // Physical vhost this conn came in on
    base_server: Pserver_rec;
    vhost_lookup_data: pointer; // used by http_vhost.c
    local_addr: Papr_sockaddr_t;  // local address

    // remote address; this is the end-point of the next hop, for the address
    //  of the request creator, see useragent_addr in request_rec
    client_addr: Papr_sockaddr_t;

    // Client's IP address; this is the end-point of the next hop, for the
    //  IP of the request creator, see useragent_ip in request_rec
    client_ip: PAnsichar;

    // Client's DNS name, if known.  NULL if DNS hasn't been checked,
    //  "" if it has and no address was found.  N.B. Only access this though
    //  get_remote_host() */
    remote_host: PAnsichar;

    // Only ever set if doing rfc1413 lookups.  N.B. Only access this through
    //  get_remote_logname() */
    remote_logname: PAnsichar;

    // server IP address
    local_ip: PAnsichar;

    // used for ap_get_server_name when UseCanonicalName is set to DNS
    //  (ignores setting of HostnameLookups) */
    local_host: PAnsichar;
    id: integer; // ID of this connection; unique at any point in time

    // Config vector containing pointers to connections per-server
    //  config structures.
    conn_config: Pap_conf_vector_t;

    // Notes on *this* connection: send note from one module to
    //  another. must remain valid for all requests on this conn */
    notes: Papr_table_t;
    input_filters: Pap_filter_t;  // A list of input filters to be used for this connection
    output_filters: Pap_filter_t; // A list of output filters to be used for this connection
    sbh: pointer; // handle to scoreboard information for this connection
    bucket_alloc: Papr_bucket_alloc_t; // The bucket allocator to use for all bucket/brigade creations
    cs: Pconn_state_t; // The current state of this connection; may be NULL if not used by MPM
    data_in_input_filters: integer; // Is there data pending in the input filters?
    data_in_output_filters: integer; // Is there data pending in the output filters?

    // Are there any filters that clogg/buffer the input stream, breaking
    //  the event mpm.
    clogging_input_filters: cardinal;  // :1

    // Have we done double-reverse DNS?
    //  -1 yes/failure,
    //   0 not yet,
    //   1 yes/success
    double_reverse: integer;           // :2

    aborted: cardinal; // Are we still talking?

    // Are we going to keep the connection alive for another request?
    // @see ap_conn_keepalive_e
    keepalive: ap_conn_keepalive_e;
    keepalives: integer; // How many times have we used it?

    // Optional connection log level configuration. May point to a server or
    //  per_dir config, i.e. must be copied before modifying */
    log: Pap_logconf;

    // Id to identify this connection in error log. Set when the first
    //  error log entry for this connection is generated.
    log_id: PAnsichar;

    // This points to the current thread being used to process this request,
    // over the lifetime of a request, the value may change. Users of the connection
    // record should not rely upon it staying the same between calls that involve
    // the MPM.
    {$ifdef APR_HAS_THREADS}
      current_thread: Papr_thread_t;
    {$endif}
  end;

// @brief Structure used to build the config tree.
// The config tree only stores
// the directives that will be active in the running server.  Directives
// that contain other directions, such as <Directory ...>; cause a sub-level
// to be created, where the included directives are stored.  The closing
// directive (</Directory>) is not stored in the tree.
Pap_directive_t = ^ap_directive_t;
ap_directive_t = record
    directive: PAnsichar; // The current directive

    // The arguments for the current directive, stored as a space
    //  separated list */
    args: PAnsichar;
    next: Pap_directive_t; // The next directive node in the tree
    first_child: Pap_directive_t; // The first child node of this directive
    parent: Pap_directive_t; // The parent node of this directive
    data: pointer; // directive's module can store add'l data here

    // these may go away in the future, but are needed for now
    // The name of the file this directive was found in */
    filename: PAnsichar;
    line_num: integer;  // The line number the directive was on

    // A short-cut towards the last directive node in the tree.
    //  The value may not always be up-to-date but it always points to
    //  somewhere in the tree, nearer to the tail.
    //  This value is only set in the first node
    last: Pap_directive_t;
  end;

// Common structure for reading of config files / passwd files etc.
Pap_configfile_t = ^ap_configfile_t;
ap_configfile_t = record
    getch: function( ch: PAnsichar; param: pointer): apr_status_t; cdecl; // an apr_file_getc()-like function
    getstr: function( buf: pointer; bufsiz: apr_size_t; param: pointer): apr_status_t; cdecl;    // an apr_file_gets()-like function
    close: function( param: pointer): apr_status_t; cdecl;   // a close handler function
    param: pointer;  // the argument passed to getch/getstr/close
    name: PAnsichar;   // the filename / description
    line_number: cardinal;   // current line number, starting at 1
  end;

// This structure is passed to a command which is being invoked,
// to carry a large variety of miscellaneous data which is all of
// use to *somebody*...
Pcmd_parms = ^cmd_parms;
Pcommand_rec = ^command_rec;
cmd_parms = record
    info: pointer; // Argument to command from cmd_table
    override: integer;   // Which allow-override bits are set
    override_opts: integer; // Which allow-override-opts bits are set
    override_list: Papr_table_t; // Table of directives allowed per AllowOverrideList
    limited: apr_int64_t; // Which methods are <Limit>ed
    limited_xmethods: apr_array_header_t; // methods which are limited
    xlimited: Pap_method_list_t; // methods which are xlimited
    config_file: Pap_configfile_t; // Config file structure.
    directive: Pap_directive_t; // the directive specifying this command
    pool: Papr_pool_t; // Pool to allocate new storage in

    // Pool for scratch memory; persists during configuration, but
    //  wiped before the first request is served...  */
    temp_pool: Papr_pool_t;
    server: Pserver_rec; // Server_rec being configured for

    // If configuring for a directory, pathname of that directory.
    //  NOPE!  That's what it meant previous to the existence of <Files>,
    // <Location> and regex matching.  Now the only usefulness that can be
    // derived from this field is whether a command is being called in a
    // server context (path == NULL) or being called in a dir context
    // (path != NULL).  */
    path: PAnsichar;
    cmd: Pcommand_rec; // configuration command
    context: Pap_conf_vector_t;  // per_dir_config vector passed to handle_command
    err_directive: Pap_directive_t; // directive with syntax error
  end;

// How the directives arguments should be parsed.
// @remark Note that for all of these except RAW_ARGS, the config routine is
//      passed a freshly allocated string which can be modified or stored
//      or whatever...
cmd_how = (
    RAW_ARGS,           // cmd_func parses command line itself
    TAKE1,              // one argument only
    TAKE2,              // two arguments only
    ITERATE,            // one argument, occuring multiple times
                        // (e.g., IndexIgnore)
    ITERATE2,           // two arguments, 2nd occurs multiple times
                        // (e.g., AddIcon)
    FLAG,               // One of 'On' or 'Off'
    NO_ARGS,            // No args at all, e.g. </Directory>; *
    TAKE12,             // one or two arguments
    TAKE3,              // three arguments only
    TAKE23,             // two or three arguments
    TAKE123,            // one, two or three arguments
    TAKE13,             // one or three arguments
    TAKE_ARGV);         // an argc and argv are passed

// All the types of functions that can be used in directives
cmd_func = record
  case integer of
    0: // function to call for a no-args
     (no_args: function( const parms: Pcmd_parms; mconfig: pointer): PAnsiChar; cdecl);

    1: // function to call for a raw-args
     (raw_args: function( const parms: Pcmd_parms; const mconfig: pointer; args: PAnsichar): PAnsichar; cdecl;);

    2: // function to call for a argv/argc *
    (take_argv: function( const parms: Pcmd_parms; mconfig: pointer;
                          argc: integer; argv: array of PAnsichar): PAnsichar; cdecl);

    3: // function to call for a take1 *
    (take1: function( const parms: Pcmd_parms; const mconfig: pointer; w: PAnsiChar): PAnsichar; cdecl);

    4: // function to call for a take2
    (take2: function( const parms: Pcmd_parms; mconfig: pointer; w, w2: PAnsichar): PAnsichar; cdecl);

    5: // function to call for a take3
    (take3: function( const parms: Pcmd_parms; mconfig: pointer; w, w2, w3: PAnsichar): PAnsichar; cdecl);

    6: // function to call for a flag
    (flag: function( const parms: Pcmd_parms; mconfig: pointer; on: integer): PAnsichar; cdecl);
  end;

// The command record structure.  Modules can define a table of these
// to define the directives it will implement.
command_rec = record
    name: PAnsiChar; // Name of this command
    func: cmd_func; // The function to be called when this directive is parsed
    cmd_data: pointer;  // Extra data, for functions which implement multiple commands...
    req_override: integer;  // What overrides need to be allowed to enable this command.
    args_how: cmd_how; // What the command expects as arguments
    errmsg: PAnsiChar; // 'usage' message, in case of syntax errors
  end;


// Module structures.  Just about everything is dispatched through
// these, directly or indirectly (through the command and handler
// tables).
Pmodule = ^module;
module = record
    // API version, *not* module version; check that module is
    // compatible with this version of the server.
    version: integer;

    // API minor version. Provides API feature milestones. Not checked
    //  during module init */
    minor_version: integer;
    module_index: integer; // Index to this modules structures in config vectors.
    name: PAnsichar; // The name of the module's C file
    dynamic_load_handle: pointer;  // The handle for the DSO.  Internal use only

    // A pointer to the next module in the list
    //  @var module_struct *next
    next: Pmodule;

    // Magic Cookie to identify a module structure;  It's mainly
    //  important for the DSO facility (see also mod_so).
    magic: cardinal;

    // Function to allow MPMs to re-write command line arguments.  This
    //  hook is only available to MPMs.
    //  @param The process that the server is running in.
    rewrite_args: procedure( var process: process_rec) ; cdecl;

    // Function to allow all modules to create per directory configuration
    //  structures.
    //  @param p The pool to use for all allocations.
    //  @param dir The directory currently being processed.
    //  @return The per-directory structure created
    create_dir_config: function( var p: apr_pool_t; dir: PAnsichar): pointer; cdecl;

    // Function to allow all modules to merge the per directory configuration
    //  structures for two directories.
    //  @param p The pool to use for all allocations.
    //  @param base_conf The directory structure created for the parent directory.
    //  @param new_conf The directory structure currently being processed.
    //  @return The new per-directory structure created

    merge_dir_config: function( var p: apr_pool_t; base_conf, new_conf: pointer): pointer; cdecl;

    // Function to allow all modules to create per server configuration
    //  structures.
    //  @param p The pool to use for all allocations.
    //  @param s The server currently being processed.
    //  @return The per-server structure created
    create_server_config: function( var p: apr_pool_t; var s: server_rec): pointer; cdecl;

    // Function to allow all modules to merge the per server configuration
    //  structures for two servers.
    //  @param p The pool to use for all allocations.
    //  @param base_conf The directory structure created for the parent directory.
    //  @param new_conf The directory structure currently being processed.
    //  @return The new per-directory structure created
    merge_server_config: function( var p: apr_pool_t; base_conf, new_conf: pointer): pointer; cdecl;

    // A command_rec table that describes all of the directives this module
    // defines.
    cmds: Pcommand_rec;

    // A hook to allow modules to hook other points in the request processing.
    //  In this function, modules should call the ap_hook_*() functions to
    //  register an interest in a specific step in processing the current
    //  request.
    //  @param p the pool to use for all allocations
    register_hooks: procedure( p: papr_pool_t); cdecl;
  end;

//    /**
//     * Lookup the remote client's DNS name or IP address
//     * @ingroup get_remote_host
//     * @param conn The current connection
//     * @param dir_config The directory config vector from the request
//     * @param type The type of lookup to perform.  One of:
//     * <pre>
//     *     REMOTE_HOST returns the hostname, or NULL if the hostname
//     *                 lookup fails.  It will force a DNS lookup according to the
//     *                 HostnameLookups setting.
//     *     REMOTE_NAME returns the hostname, or the dotted quad if the
//     *                 hostname lookup fails.  It will force a DNS lookup according
//     *                 to the HostnameLookups setting.
//     *     REMOTE_NOLOOKUP is like REMOTE_NAME except that a DNS lookup is
//     *                     never forced.
//     *     REMOTE_DOUBLE_REV will always force a DNS lookup, and also force
//     *                   a double reverse lookup, regardless of the HostnameLookups
//     *                   setting.  The result is the (double reverse checked)
//     *                   hostname, or NULL if any of the lookups fail.
//     * </pre>
//     * @param str_is_ip unless NULL is passed, this will be set to non-zero on output when an IP address
//     *        string is returned
//     * @return The remote hostname
function ap_get_remote_host
  (var conn: conn_rec; dir_config: Pointer; _type: Integer; var str_is_ip: integer): PAnsiChar; stdcall;
  external libhttpd name '_ap_get_remote_host@16';

function ap_get_server_port
  (var r: request_rec): apr_port_t; stdcall;
  external libhttpd name '_ap_get_server_port@4';

function  ap_get_server_name
  (var r: request_rec): PAnsiChar; stdcall;
   external libhttpd name '_ap_get_server_name@4';

function ap_get_server_banner: PAnsiChar; cdecl;
  external libhttpd name '_ap_get_server_banner@0';

function apr_table_get
  (var t: apr_table_t; key: PAnsiChar): PAnsiChar; stdcall;
  external libapr_1 name '_apr_table_get@8';

procedure apr_table_set
  (var t: apr_table_t; key, val: PAnsiChar); stdcall;
  external libapr_1 name '_apr_table_set@12';

procedure apr_table_add
  (var t: apr_table_t; key, val: PAnsiChar); stdcall;
  external libapr_1 name '_apr_table_add@12';

type TCompFunc = function( P: Pointer; PC, PC2: PAnsichar): integer; cdecl;
procedure apr_table_vdo
  (comp: TCompFunc; rec: Pointer; var t: apr_table_t; list: va_list); stdcall;
  external libapr_1 name '_apr_table_vdo@16';


// Functions from http_protocol.h
function ap_setup_client_block
  (var r: request_rec; read_policy: integer): integer; stdcall;
  external libhttpd name '_ap_setup_client_block@8';

function ap_should_client_block
  (var r: request_rec): integer; stdcall;
  external libhttpd name '_ap_should_client_block@4';

function ap_get_client_block( var r:  request_rec; buffer: PAnsiChar; bufsiz: apr_size_t): LongInt; stdcall;
  external libhttpd name '_ap_get_client_block@12';

//   Write a buffer for the current request
//   @param buf The buffer to write
//   @param nbyte The number of bytes to send from the buffer
//   @param r The current request
//   @return The number of bytes sent
function ap_rwrite
  (var buf; nbyte: integer; var r: request_rec): integer; stdcall;
  external libhttpd name '_ap_rwrite@12';


// From apr_strings.h
function apr_pstrdup
  (var p: apr_pool_t; s: PAnsiChar): PAnsiChar; stdcall;
  external libapr_1 name '_apr_pstrdup@8';


// Run the header parser functions for each module
// @param r The current request
// @return OK or DECLINED
type
  ap_HOOK_header_parser_t = function( var r: request_rec): integer; cdecl;
  ap_HOOK_handler_t = function( var r: request_rec): Integer; cdecl;

// From http_config.h
procedure ap_hook_header_parser
  (pf: ap_HOOK_header_parser_t; aszPre, aszSucc: PPAnsiChar; nOrder: integer); stdcall;
  external libhttpd name '_ap_hook_header_parser@16';

procedure ap_hook_handler
  (pf: ap_HOOK_handler_t; aszPre, aszSucc: PPAnsiChar; nOrder: integer); stdcall;
  external libhttpd name '_ap_hook_handler@16';

function ap_server_root_relative
  (p: Papr_pool_t; fname: PAnsiChar): PAnsiChar; stdcall;
  external libhttpd name '_ap_server_root_relative@8';


// Create and initialize a mutex that can be used to synchronize threads.
// @param mutex the memory address where the newly created mutex will be
//        stored.
// @param flags Or'ed value of:
// <PRE>
//           APR_THREAD_MUTEX_DEFAULT   platform-optimal lock behavior.
//           APR_THREAD_MUTEX_NESTED    enable nested (recursive) locks.
//           APR_THREAD_MUTEX_UNNESTED  disable nested locks (non-recursive).
// </PRE>
// @param pool the pool from which to allocate the mutex.
// @warning Be cautious in using APR_THREAD_MUTEX_DEFAULT.  While this is the
// most optimial mutex based on a given platform's performance charateristics,
// it will behave as either a nested or an unnested lock.
type
Tapr_thread_mutex_Flags = (
  APR_THREAD_MUTEX_DEFAULT  = $0,   // platform-optimal lock behavior
  APR_THREAD_MUTEX_NESTED   = $1,   // enable nested (recursive) locks
  APR_THREAD_MUTEX_UNNESTED = $2);  // disable nested locks

function apr_thread_mutex_create(
  var mutex: Papr_thread_mutex_t; flags: Tapr_thread_mutex_Flags; // Cardinal
  pool: Papr_pool_t): apr_status_t; stdcall;
  external libapr_1 name '_apr_thread_mutex_create@12';

function ap_rputs( str: PAnsiChar; var r: request_rec): Integer; stdcall;
// From version 2.3.13-dev, this was changed from an export function to an inline + Macro.

function ap_rputs_DelphiU( const str: utf8string; var r: request_rec): integer;
function ap_rputs_DelphiA( const str: ansistring; var r: request_rec): integer;
function ap_rputs_DelphiW( const str: string; var r: request_rec): integer;


implementation
uses SysUtils;



function ap_rputs( str: PAnsiChar; var r: request_rec): Integer; stdcall;
begin
result := ap_rwrite( str^, Strlen( str), r)
end;

function ap_rputs_DelphiU( const str: utf8string; var r: request_rec): integer;
var
  Buf: PAnsiChar;
begin
Buf := PAnsiChar( str);
result := ap_rwrite( Buf^, Length( str), r)
end;

function ap_rputs_DelphiA( const str: ansistring; var r: request_rec): integer;
var
  Buf: PAnsiChar;
begin
Buf := PAnsiChar( str);
result := ap_rwrite( Buf^, Length( str), r)
end;

function ap_rputs_DelphiW( const str: string; var r: request_rec): integer;
begin
result := ap_rputs_DelphiU( UTF8Encode( str), r)
end;

end.
