# Standard library
from hashlib import sha1
from urlparse import urlparse, parse_qs, parse_qsl, urlunsplit
from urllib import urlencode

# 3rd party library
from flask import Flask, flash, request, render_template, redirect, url_for

app = Flask(__name__)
app.secret_key = 'some secret'

DB = {}
counter = {}

def clean_url(url):
    url = urlparse(url)
    query = urlencode(sorted(parse_qsl(url.query)))
    data = url.scheme, url.netloc, url.path, query, url.fragment
    return urlunsplit(data)

def get_url_from_db(url_id):
    return DB.get(url_id)

def hash_url(url):
    return sha1(clean_url(url)).hexdigest()[:10]

def increment_count(hash):
    count = counter.setdefault(hash, 0)
    counter[hash] += 1

@app.route('/')
def index():
    return render_template('index.html')

@app.route('/shorten', methods=['POST'])
def shorten():
    url = request.form.get('url', None)
    # FIXME: Add checks to see if URL is valid.
    # FIXME: May be follow redirects, also
    if url is None or len(url.strip()) == 0:
        flash('Need a url to shorten!')
        return redirect(url_for('index'))

    url_id = hash_url(url)
    DB.setdefault(url_id, url)
    short_url = request.host_url + url_id
    return render_template('shortened.html', url=url, short_url=short_url)


@app.route('/<hash>')
def longen(hash):
    url = get_url_from_db(hash)
    if url is not None:
        increment_count(hash)
        return redirect(url)
    else:
        flash('No such short url found')
        return redirect(url_for('index'))

if __name__ == "__main__":
    app.run(host='0.0.0.0', debug=True)
