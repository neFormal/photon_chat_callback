# dependencies:
1. erlang 16b02 with libssl, libncurses
2. python2.7, python-pip, python-dev

# deploy order:
1. pip install -r reqs.txt
2. fab deploy
3. fab start
3. when you add new nodes you should call something like this with your new host
`fab -H server2.myhosting.com add_node`

for localhost you can use ngrok/proxylocal but you can get some troubles with fabric
