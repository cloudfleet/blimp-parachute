                             ━━━━━━━━━━━━━━
                                 README


                              Mark Evenson
                             ━━━━━━━━━━━━━━


Table of Contents
─────────────────

1 Installation
.. 1.1 Place chute.asd in ASDF3 system registry
.. 1.2 Loading from ASDF
.. 1.3 Testing from ASDF
.. 1.4 Loading Quicklisp dependencies
2 REST API
.. 2.1 Current version 'v1'
.. 2.2 Basic transfer of opaque binary data
3 Client platform arm32
.. 3.1 CCL
4 Server platform x64
.. 4.1 SBCL
5 What is to be DONE
.. 5.1 DONE BTRFS volume must have ".snapshot/" directory
.. 5.2 DONE make a hard link to btrfs, setuid
.. 5.3 BTRFS/SEND
..... 5.3.1 DONE stdout/stderr mixing
..... 5.3.2 DONE Use octet streams
.. 5.4 DONE parse id for PUT uri from POST
.. 5.5 TODO Read key from key device
.. 5.6 TODO Initialize nonce from random data
.. 5.7 TODO Read domain from /opt/cloudfleet/data/config/blimp-vars.sh
.. 5.8 DONE MAKE-NEW-DIRECTORY
.. 5.9 TODO Verify basic transfer
..... 5.9.1 REST Transfer Implementation
..... 5.9.2 Tests of transfer integrity
.. 5.10 TODO Future interface for subaddressing components of a blob
.. 5.11 (at first without byte ranges).
6 Colophon


#+TITLE Parachute: a zero knowledge backup system


1 Installation
══════════════

  Running
  ┌────
  │ bash -x setup/install-parachute.bash
  └────
  with the appropiate permissions should enable all the following
  subclauses to be met.


1.1 Place chute.asd in ASDF3 system registry
────────────────────────────────────────────

  ┌────
  │ mkdir -p ~/.config/common-lisp/source-registry.conf.d/
  │ cp chute.conf ~/.config/common-lisp/source-registry.conf.d/
  └────


1.2 Loading from ASDF
─────────────────────

  ┌────
  │ (asdf:load-system :chute)
  └────


1.3 Testing from ASDF
─────────────────────

  ┌────
  │ (asdf:test-system :chute)
  └────


1.4 Loading Quicklisp dependencies
──────────────────────────────────

  ┌────
  │ wget https://beta.quicklisp.org/quicklisp.lisp
  │ (load "~/quicklisp.lisp")
  └────
  ┌────
  │ (load (asdf:system-relative-pathname 
  │    (asdf:find-system :chute) "quicklisp-setup"))
  └────


2 REST API
══════════

2.1 Current version 'v1'
────────────────────────

  ┌────
  │ POST /chute/blob/
  │ (index.json)                
  │ ->>   201 Resource Created or [45]xx Error
  │ ("/new/uri/to/use")
  │ 
  │ 
  │ GET /<URI>/index.json              
  │ nil
  │ ->>   200 Original or 304 Not Modified or [45]xx Error
  │ (index.json) or nil
  │ 
  │ 
  │ PUT /<URI>/0    
  │ (application/octet-bytes)
  │ ->>   201 Ok
  │ (json) "true" or "false" 
  │ 
  │ GET /<URI>/0/hash/sha256 
  │ ->>   200 
  │ (json) SHA256 Hash
  └────

  Authentication to be handled via HTTP Headers submitted to
  ```spire.marina.io``` API.


2.2 Basic transfer of opaque binary data
────────────────────────────────────────

  PUT with 'Byte-range' headers n bytes in m windows.  Use timing of
  previous sessions to dynamically expand/contract number of bytes in
  each PUT request.

  Each chunk of a blob is adressed as a URI of the form:


  …/<domain>/<node>/<mount-path>/<timestamp>/<n-byte-chunks>/<mth-chunk>


  MIME type for PUT content is "application/octet-stream".


3 Client platform arm32
═══════════════════════

3.1 CCL
───────

  [http://trac.clozure.com/ccl/wiki/PlatformNotes]


4 Server platform x64
═════════════════════

4.1 SBCL
────────

  sbcl-1.3.1 known to work


5 What is to be DONE
════════════════════

5.1 DONE BTRFS volume must have ".snapshot/" directory
──────────────────────────────────────────────────────

  • CLOSING NOTE [2015-12-14 Mon 13:55]
         Should have been completed in the setup procedure.  File issue
    if found to be otherwise.
  The CHUTE:SNAPSHOT command will create snapshots under a the specified
  volume (by default "/opt/cloudfleet/data") in a sub-directory named
  ".snapshot/", i.e. for the default in
  "/opt/cloudfleet/data/.snapshot/<TIMESTAMP>".

  Therefore, the "/opt/cloudfleet/data/.snapshot/" must exist and be
  owned by root.  Currently this is done once per node as part of
  installation when "setup/add-subvolume.bash" is executed.  It should
  be made part of the setup done by setting up the encrypted volumes.


5.2 DONE make a hard link to btrfs, setuid
──────────────────────────────────────────

  • CLOSING NOTE [2015-10-27 Tue 12:44]
         Done as setup/make-suid-btfs.bash


5.3 BTRFS/SEND
──────────────

5.3.1 DONE stdout/stderr mixing
╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌

  • CLOSING NOTE [2015-11-14 Sat 10:16]
          Fixed by specifying separate error/output arguments to
          CCL:RUN-PROGAM
  SBCL/CCL returning different starts of output stream.  CCL contains
  "At subvolume" which means it is mixing stdout/stderr?


5.3.2 DONE Use octet streams
╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌

  • CLOSING NOTE [2015-11-19 Thu 10:47]
          Resolved by use of octets streams directly on MAKE-BLOB
  We should pass the streams we wish to read from, rather than having
  RUN-PROGAM construct them for us.

  Unfortunately, this doesn't seem to work:
  ┌────
  │ (ccl:run-program
  │    "/bin/ls" nil
  │    :wait nil
  │    :output (ironclad:make-octet-input-stream
  │            (make-array 16 :element-type '(unsigned-byte 8)))
  │    :element-type '(unsigned-byte 8))
  └────

  complaining about

  There is no applicable method for the generic function:
    #<STANDARD-GENERIC-FUNCTION STREAM-WRITE-VECTOR #x30200006518F>
  when called with arguments:
    (#<IRONCLAD::OCTET-INPUT-STREAM #x3020023CA75D> "chute
  LICENSE
  parachute.org
  setup
  ...

  Fortunately, CCL:RUN-PROGRAM takes an :ELEMENT-TYPE keyword which
  allows us to get octet streams out of the subprocesses.


5.4 DONE parse id for PUT uri from POST
───────────────────────────────────────

  • CLOSING NOTE [2015-12-14 Mon 14:09]
         Finished with proper implementation of POST followed by
    subsequent PUTs to the unspecified sub-uri namespace.


5.5 TODO Read key from key device
─────────────────────────────────

  Propsoal: extract appropiate number of bytes from SHA256 of the LuKS
  key


5.6 TODO Initialize nonce from random data
──────────────────────────────────────────

  Done.  But needs to be verified via a test.


5.7 TODO Read domain from /opt/cloudfleet/data/config/blimp-vars.sh
───────────────────────────────────────────────────────────────────

  Best implementation: exec a bash process then read its environment
  table


5.8 DONE MAKE-NEW-DIRECTORY
───────────────────────────

  • CLOSING NOTE [2015-12-13 Sun 10:40]
         Use CL-FAD routines instead of ASDF.
  Fails until run from the REPL.  Unsure what this entails.


5.9 TODO Verify basic transfer
──────────────────────────────

  Basic transfer of backups needs to be implemented completely and
  tested:


5.9.1 REST Transfer Implementation
╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌

  Initial mplementation completed.  Mocks in place for many other
  systems.


5.9.2 Tests of transfer integrity
╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌

◊ 5.9.2.1 CHUTE.TEST::TRANSFER.BLOB.1

  Transform a given file into a blob


◊ 5.9.2.2 CHUTE.TEST::TRANSFER.BLOB.2

  Use results of BTRFS/SEND into a blob


5.10 TODO Future interface for subaddressing components of a blob
─────────────────────────────────────────────────────────────────

  For resumable transfers

  ┌────
  │ PUT /<URI>/0/<chunk-bytes>/<nth-chunk>
  │ ->>   201 on success or [345]00 
  │   (json) "true" or "false"
  │ 
  │ GET /<URI>/0/<chunk-bytes>/<nth-chunk>/hash/sha256  
  │ ->>   20x [345]xx (does 314 make sense?)
  │   (json) SHA256 Hash
  └────


5.11 (at first without byte ranges).
────────────────────────────────────

  Implementation use HTTP 'Byte-range' header to files
  attached/detatched via mmap().


6 Colophon
══════════

  <mark@evenson.eu>
  Created: 01-OCT-2015
  Revised: 16-DEC-2015
