import pkg_resources

def find_format_as_stream(extension):
    return pkg_resources.resource_stream('bitpunch.resources',
                                         'bp/{0}.bp'.format(extension))
