ALE _Hub_
==========

_ALE Hub_ is the piece of software operated by the entity that manages ALE.

This is a privileged node that sits in the center of the _ALE hive_ and deals
with matters that require authority such as user management and fiat transactions.


----------------------


While the hive is running in “centralised” mode it is expected that
all nodes will be connecting to the Hub instead of maintaining p2p connections
to each other. This will allow the Hub to single-handedly control what gets
onto the blockchain instead of requiring a consensus of nodes.
