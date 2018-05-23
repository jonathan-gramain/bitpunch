#
# Copyright (c) 2017, Jonathan Gramain <jonathan.gramain@gmail.com>. All
# rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# * Redistributions of source code must retain the above copyright
#   notice, this list of conditions and the following disclaimer.
# * Redistributions in binary form must reproduce the above copyright
#   notice, this list of conditions and the following disclaimer in the
#   documentation and/or other materials provided with the distribution.
# * The names of the bitpunch project contributors may not be used to
#   endorse or promote products derived from this software without
#   specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
# A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
# HOLDERS AND CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
# BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
# OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
# ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
# TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
# USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
# DAMAGE.
#

import pkg_resources

# TODO bp resources should expose their own support per extension
extension_to_path = {
    'mp4': 'media.video.mp4',
    'ogg': 'media.container.ogg',
    'tar': 'archive.ustar',
    'ldb': 'database.leveldb.ldb',
}

mimetype_to_path = {
    'application/ogg': 'media.container.ogg',
    'application/tar': 'archive.ustar',
    'application/x-tar': 'archive.ustar',
    'video/mp4': 'media.video.mp4',
}

def find_handler(**kwargs):
    """find a bitpunch handler by various criteria

    Parameters
    ----------
    Pass one of these parameters:

    path: path to the format handler, with dots separating components
          and without the '.bp' extension (e.g. 'media.container.ogg')
    extension: file extension (e.g. 'ogg')
    mimetype: mime type (e.g. 'application/ogg')

    """
    if not kwargs:
        raise ValueError('require keyword argument')
    handler_path = None
    if handler_path is None and 'path' in kwargs:
        handler_path = kwargs['path']
        if (len(handler_path) == 0 or
            handler_path.startswith('.') or
            '/' in handler_path):
            raise ValueError('"path" keyword argument is not a valid path')
    if handler_path is None and 'mimetype' in kwargs:
        try:
            handler_path = mimetype_to_path[kwargs['mimetype']]
        except KeyError:
            pass
    if handler_path is None and 'extension' in kwargs:
        try:
            handler_path = extension_to_path[kwargs['extension'].lower()]
        except KeyError:
            pass
    if handler_path is not None:
        bp_path = 'bp/' + handler_path.replace('.', '/') + '.bp'
        try:
            return pkg_resources.resource_stream('bitpunch.resources', bp_path)
        except Exception:
            return None
    return None
