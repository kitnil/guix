pipeline {
    agent {
        label "master"
    }
    environment {
        LOCAL_WORKTREE = "/home/oleg/src/guix"
        MASTER_WORKTREE = "${LOCAL_WORKTREE}-master"
        BUILD_SCRIPT = '''
#!/bin/sh
env GUIX_PACKAGE_PATH= guix environment --pure guix                                     \
    --ad-hoc help2man guile-sqlite3 guile-gcrypt                                        \
    -- sh -c "set -e -x; ./bootstrap; ./configure --localstatedir=/var --prefix=; make"
'''
    }
    stages {
        stage("Pulling from upstream Git") {
            steps {
                dir(LOCAL_WORKTREE) {
                    sh "git pull --rebase upstream"
                }
                dir(MASTER_WORKTREE) {
                    sh "git pull --rebase upstream"
                }
            }
        }
        stage("Cloning from local Git") {
            steps {
                parallelGitClone url: "https://cgit.duckdns.org/git/guix/guix",
                branch: "wip-local", nodeLabels: ["guix"], dir: LOCAL_WORKTREE
            }
        }
        stage("Invoking guix pull") {
            steps {
                parallelSh cmd: "guix pull --channels=${LOCAL_WORKTREE}/channels.scm",
                nodeLabels: ["guix"]
            }
        }
        stage("Invoking guix pull as root") {
            steps {
                parallelSh cmd: "sudo -i guix pull --channels=${LOCAL_WORKTREE}/channels.scm",
                nodeLabels: ["guix"]
            }
        }
        stage("Building from Git") {
            steps {
                parallelSh cmd: BUILD_SCRIPT, nodeLabels: ["guix"], dir: "/home/oleg/src/guix"
            }
        }
        stage("Building from master") {
            steps {
                dir(MASTER_WORKTREE) {
                    sh BUILD_SCRIPT
                }
            }
        }
    }
}
