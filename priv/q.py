#!/usr/bin/env python3

import os,sys,json,time,re
from datetime import datetime,date
from http import client as hc
from colorama import Fore,Style


HOST='127.0.0.1'
PORT=7000
TIME_RE = r"((?:\d\d\.)?(?:\d\d)\.(?:\d\d))(?:([+-])?(\d+)[dD][aA][yY])?"
SESSION_RE = r"([^_]+)_(\d+)_(\d+)"
SIG_INVITE_RE = r"^(INVITE)\s*(\S+)(\s*SIP/2\.0)$"
SIG_HEADER_RE = r"^([^:]+)\:(.+)$"


DAY_MILLISECONDS = 86_400_000


def usage():
    print('''
    q.py   <method>  <query_id>  <time_range>
    method:  (session | s) | (caller | c) | (callee | e)
    query_id:   session_id | caller_num | callee_num
    time_range:   yy.mm.dd[Nday] |  mm.dd[Nday]

    Example:
        q.py session some_session  04.12
        q.py caller 123456789   04.12-8day
        q.py callee 987654321   04.12+8day
    ''')

def timestamp(time_range):
    def parse_date(dstr):
        if (len(dstr) == 5):     # year is not included
            year = date.today().year
            (month,day) = dstr.split('.')
        else:
            (year,month,day) = dstr.split('.')
        dargs = [int(x) for x in (year,month,day)]
        return int(datetime(*dargs).timestamp() * 1000)

    m = re.match(TIME_RE,time_range)
    if m is None:
        raise Exception('invalid time range')
    elif len(m.groups()) == 3:
        start = parse_date(m.group(1))
        sign = m.group(2)
        days = m.group(3)
        if sign is None:
            tsend = start + DAY_MILLISECONDS # within 1 day
        elif sign == '+':
            tsend = start + int(days) * DAY_MILLISECONDS
        elif sign == '-':
            tsend = start - int(days) * DAY_MILLISECONDS
        else:
            raise Exception('wrong sign in time_range')
        return sorted((start,tsend))
    else:
        raise Exception('timestamp error')

def url(method,query_id,time_range):
    ts = timestamp(time_range)
    return 'http://%s:%d/sip/%s?id=%s&ts_start=%s&ts_end=%s' % (HOST,PORT,method,query_id,ts[0],ts[1])

def format_session_name(leveldb_key):
    m = re.match(SESSION_RE,leveldb_key)
    if m is None:
        return None
    else:
        session = m.group(1)
        seq = int(m.group(2))
        timestamp = int(m.group(3))
        date = datetime.fromtimestamp(timestamp // 1000)
        date_str = date.strftime('[%m/%d] %H:%M:%S')
        return Fore.CYAN + '|%s<%d>|  %s' % (session,seq,date_str)

def format_signalling(sig_line):
    color1 = Style.BRIGHT
    color2 = Style.NORMAL + Fore.WHITE
    m = re.match(SIG_INVITE_RE,sig_line)
    if m is not None:
        color1 = Style.BRIGHT + Fore.RED
        color2 = Style.BRIGHT + Fore.YELLOW
        return color1 + m.group(1) + ' ' + color2 + m.group(2) + \
            Style.RESET_ALL + Fore.RESET + m.group(3)
    else:
        m = re.match(SIG_HEADER_RE,sig_line)
        if m is None:
            return sig_line
        else:
            header = m.group(1).lower()
            if header == 'from' or header == 'to':
                color1 += Fore.GREEN
            elif '=' in header: # sdp properties
                color1 = Style.NORMAL
            return color1 + m.group(1) + ':' + color2 + m.group(2)

def query(method,query_id,time_range):
    req_url = url(method,query_id,time_range)
    #print('request url is ',req_url)
    conn = hc.HTTPConnection(HOST,PORT)
    conn.request('GET',req_url)
    resp = conn.getresponse()
    if resp.status == 200:
        data = json.loads(resp.read())
        pretty_print(data)

def pretty_print(session_list):
    for l in session_list:
        signal = l['signal']
        for name,data in signal.items():
            formatted_signal = '\r\n'.join((format_signalling(s) for s in data.split('\r\n')))
            print(format_session_name(name))
            print(formatted_signal)

def main():
    try:
        (method,query_id,time_range) = sys.argv[1:4]
        if method == 'session' or method == 's':
            query('get_session',query_id,time_range)
        elif method == 'caller' or method == 'c':
            query('get_caller',query_id,time_range)
        elif method == 'callee' or method == 'e':
            query('get_callee',query_id,time_range)
        else:
            raise Exception('invalid method')
    except Exception as e:
        print(e.with_traceback())
        usage()


if __name__ == '__main__':
    main()
