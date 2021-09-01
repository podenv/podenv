# UserNS settings

This document explains how the user namespace is set.

> Checkout this article: https://www.redhat.com/sysadmin/debug-rootless-podman-mounted-volumes

## Podman

Podenv uses these strategies depending on the capabilities set:

- `root`: start container with `--user 0`
- `hostuser`: start container with `--user $(id -u) --userns keep-id`
- `anyuser`: start container with `--user $RAND --uidmap $RAND:0:1 --uidmap 0:1:$RAND`

When sharing host files, we need to tweak the userns. Here is a helper script:

```bash
podman-mount () {
  test -d /tmp/test && rm -Rf /tmp/test
  mkdir -m 1777 -p /tmp/test
  chcon system_u:object_r:container_file_t:s0 /tmp/test
  set -x
  podman run --volume /tmp/test:/tmp/test $@ --rm ubi8 touch /tmp/test/file
  set +x
  ls -la /tmp/test/file
}
```

Here are the different use-case:

- `--user 0`: ok
- `--user 1000`: ko, file uid is 100999
- `--user 4242`: ko, file uid id 104241

- `--user 0    --userns keep-id`: ko, file uid is 100000
- `--user 1000 --userns-keep-id`: ok
- `--user 4242 --userns keep-id`: ko, file uid is 104241

- `--user 0    --uidmap 4242:0:1 --uidmap 0:1:4242 --uidmap 4243:4243:61294`: ko, file uid is 100000
- `--user 1000 --uidmap 4242:0:1 --uidmap 0:1:4242 --uidmap 4243:4243:61294`: ko, file uid is 101000
- `--user 4242 --uidmap 4242:0:1 --uidmap 0:1:4242 --uidmap 4243:4243:61294`: ok
