import os
from fabric.api import *

import sys
reload(sys)
sys.setdefaultencoding('utf-8')
env.colorize_errors = True


env.hosts = ['localhost']
#env.roledefs['web'] = ['localhost']
env.project_root = "/home/user/deploy"
env.admin_node = 'callback@127.0.0.1'

@task
#@roles('web')
def deploy():
    mkdirs()
    local('./rebar get-deps')
    local('./rebar compile')
    local('./rebar generate')
    with lcd('rel'):
        local('tar zcvf callback.tar.gz callback')
        put('callback.tar.gz', os.path.join(env.project_root, 'callback.tar.gz'))
        local('rm callback.tar.gz')
    with cd(env.project_root):
        run('tar zxvf callback.tar.gz')

@task
#@roles('web')
def mkdirs():
    run('mkdir -p ' + env.project_root)

@task
def start():
    with cd(env.project_root):
        run('./callback/bin/callback start')

@task
def add_node():
    local("""./erl_call -c callback -v -a "callback_app enable_node ['%s']" -n 'callback@%s'""" % (env.admin_node, env.host))
