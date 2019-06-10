import web
import time

urls = (
    '/', 'index',
    '/data', 'storeData',
)


class index:
    def GET(self):
        return "Hello, world!"


class storeData:
    def GET(self):
        return "Hello, world!"

    def POST(self):
        i = web.input(data='', site='')
        # web.debug(i.data)
        f = open('data/' + i.site.replace('/',
                                          '').replace(':', '') + '.json', 'w')
        f.write(i.data)
        f.close()
        return '{"success":"OK"}'


if __name__ == "__main__":
    app = web.application(urls, globals())
    app.run()
