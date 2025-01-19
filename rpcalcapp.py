from bottle import Bottle, run, static_file

app = Bottle()

@app.route('/static/<filepath:path>')
def server_static(filepath):
    return static_file(filepath, root='./web/static/')

if __name__ == '__main__':
    app.run(host='localhost', port=9090)