# SPDX-FileCopyrightText: 2021 Arthur Breitman
# SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

class AutoRepr(object):
    def __repr__(self):
        items = ("%s = %r" % (k, v) for k, v in self.__dict__.items())
        return "<%s: {%s}>" % (self.__class__.__name__, ', '.join(items))
