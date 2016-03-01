/* ----------------------------------------------------------------------- *
 *   
 *   Copyright 1996-2014 The NASM Authors - All Rights Reserved
 *   See the file AUTHORS included with the NASM distribution for
 *   the specific copyright holders.
 *
 *   Redistribution and use in source and binary forms, with or without
 *   modification, are permitted provided that the following
 *   conditions are met:
 *
 *   * Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   * Redistributions in binary form must reproduce the above
 *     copyright notice, this list of conditions and the following
 *     disclaimer in the documentation and/or other materials provided
 *     with the distribution.
 *     
 *     THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
 *     CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
 *     INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 *     MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 *     DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
 *     CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 *     SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 *     NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 *     LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 *     HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 *     CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
 *     OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
 *     EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * ----------------------------------------------------------------------- */

/*
 * rdlib.c - routines for manipulating RDOFF libraries (.rdl)
 */

#include "compiler.h"

#include "xstdio.h"
#include <stdlib.h>
#include <string.h>

#define RDOFF_UTILS

#include "rdoff.h"
#include "rdlib.h"
#include "rdlar.h"

/* See Texinfo documentation about new RDOFF libraries format */

int rdl_error = 0;

char *rdl_errors[5] = {
    "no error", "could not open file", "invalid file structure",
    "file contains modules of an unsupported RDOFF version",
    "module not found"
};

int rdl_verify(const char *filename)
{
    XFILE *fp;
    char buf[257];
    int i;
    int32_t length;
    static char lastverified[256];
    static int lastresult = -1;

    if (lastresult != -1 && !strcmp(filename, lastverified))
        return lastresult;

    fp = xfopen(filename, "rb");
    strcpy(lastverified, filename);

    if (!fp)
        return (rdl_error = lastresult = 1);

    while (!xfeof(fp)) {
        i = 0;

        while (xfread(buf + i, 1, 1, fp) == 1 && i < 257 && buf[i])
            i++;
        if (xfeof(fp))
            break;

        if (buf[0] == '.') {
            /*
             * A special module, eg a signature block or a directory.
             * Format of such a module is defined to be:
             *   six char type identifier
             *   int32_t count bytes content
             *   content
             * so we can handle it uniformaly with RDOFF2 modules.
             */
            xfread(buf, 6, 1, fp);
            buf[6] = 0;
            /* Currently, nothing useful to do with signature block.. */
        } else {
            xfread(buf, 6, 1, fp);
            buf[6] = 0;
            if (strncmp(buf, "RDOFF", 5)) {
                xfclose(fp);
                return rdl_error = lastresult = 2;
            } else if (buf[5] != '2') {
                xfclose(fp);
                return rdl_error = lastresult = 3;
            }
        }
        xfread(&length, 4, 1, fp);
        xfseek(fp, length, SEEK_CUR);    /* skip over the module */
    }
    xfclose(fp);
    return lastresult = 0;      /* library in correct format */
}

int rdl_open(struct librarynode *lib, const char *name)
{
    int i = rdl_verify(name);
    if (i)
        return i;

    lib->fp = NULL;
    lib->name = strdup(name);
    lib->referenced = 0;
    lib->next = NULL;
    return 0;
}

int rdl_searchlib(struct librarynode *lib, const char *label, rdffile * f)
{
    char buf[512];
    int i, t;
    void *hdr;
    rdfheaderrec *r;
    int32_t l;

    rdl_error = 0;
    lib->referenced++;

    if (!lib->fp) {
        lib->fp = xfopen(lib->name, "rb");

        if (!lib->fp) {
            rdl_error = 1;
            return 0;
        }
    } else
        xrewind(lib->fp);

    while (!xfeof(lib->fp)) {
        /*
         * read the module name from the file, and prepend
         * the library name and '.' to it.
         */
        strcpy(buf, lib->name);

        i = strlen(lib->name);
        buf[i++] = '.';
        t = i;
        while (xfread(buf + i, 1, 1, lib->fp) == 1 && i < 512 && buf[i])
            i++;

        buf[i] = 0;

        if (xfeof(lib->fp))
            break;
        if (!strcmp(buf + t, ".dir")) { /* skip over directory */
            xfread(&l, 4, 1, lib->fp);
            xfseek(lib->fp, l, SEEK_CUR);
            continue;
        }
        /*
         * open the RDOFF module
         */
        if (rdfopenhere(f, lib->fp, &lib->referenced, buf)) {
            rdl_error = 16 * rdf_errno;
            return 0;
        }
        /*
         * read in the header, and scan for exported symbols
         */
        hdr = malloc(f->header_len);
        rdfloadseg(f, RDOFF_HEADER, hdr);

        while ((r = rdfgetheaderrec(f))) {
            if (r->type != 3)   /* not an export */
                continue;

            if (!strcmp(r->e.label, label)) {   /* match! */
                free(hdr);      /* reset to 'just open' */
                f->header_loc = NULL;   /* state... */
                f->header_fp = 0;
                return 1;
            }
        }

        /* find start of next module... */
        i = f->eof_offset;
        rdfclose(f);
        xfseek(lib->fp, i, SEEK_SET);
    }

    /*
     * close the file if nobody else is using it
     */
    lib->referenced--;
    if (!lib->referenced) {
        xfclose(lib->fp);
        lib->fp = NULL;
    }
    return 0;
}

int rdl_openmodule(struct librarynode *lib, int moduleno, rdffile * f)
{
    char buf[512];
    int i, cmod, t;
    int32_t length;

    lib->referenced++;

    if (!lib->fp) {
        lib->fp = xfopen(lib->name, "rb");
        if (!lib->fp) {
            lib->referenced--;
            return (rdl_error = 1);
        }
    } else
        xrewind(lib->fp);

    cmod = -1;
    while (!xfeof(lib->fp)) {
        strcpy(buf, lib->name);
        i = strlen(buf);
        buf[i++] = '.';
        t = i;
        while (xfread(buf + i, 1, 1, lib->fp) == 1 && i < 512 && buf[i])
            i++;
        buf[i] = 0;
        if (xfeof(lib->fp))
            break;

        if (buf[t] != '.')      /* special module - not counted in the numbering */
            cmod++;             /* of RDOFF modules - must be referred to by name */

        if (cmod == moduleno) {
            rdl_error = 16 *
                rdfopenhere(f, lib->fp, &lib->referenced, buf);
            lib->referenced--;
            if (!lib->referenced) {
                xfclose(lib->fp);
                lib->fp = NULL;
            }
            return rdl_error;
        }

        xfread(buf, 6, 1, lib->fp);
        buf[6] = 0;
        if (buf[t] == '.') {
            /* do nothing */
        } else if (strncmp(buf, "RDOFF", 5)) {
            if (!--lib->referenced) {
                xfclose(lib->fp);
                lib->fp = NULL;
            }
            return rdl_error = 2;
        } else if (buf[5] != '2') {
            if (!--lib->referenced) {
                xfclose(lib->fp);
                lib->fp = NULL;
            }
            return rdl_error = 3;
        }

        xfread(&length, 4, 1, lib->fp);
        xfseek(lib->fp, length, SEEK_CUR);       /* skip over the module */
    }
    if (!--lib->referenced) {
        xfclose(lib->fp);
        lib->fp = NULL;
    }
    return rdl_error = 4;       /* module not found */
}

void rdl_perror(const char *apname, const char *filename)
{
    if (rdl_error >= 16)
        rdfperror(apname, filename);
    else
        xfprintf(xstderr, "%s:%s:%s\n", apname, filename,
                rdl_errors[rdl_error]);
}
