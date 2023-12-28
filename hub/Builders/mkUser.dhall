let uid = "\${USER_UID}"

in  \(name : Text) ->
          "useradd -u ${uid} -d /home/${name} -m ${name} && "
      ++  "mkdir -p /home/${name}/.cache /home/${name}/.config /run/user/${uid} /run/user/0 && "
      ++  "chown -R ${uid}:${uid} /home/${name} /run/user/"
