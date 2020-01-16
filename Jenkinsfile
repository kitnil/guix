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
        GUIX_PULL_COMMAND = "guix pull --branch=wip-local --channels=${LOCAL_WORKTREE}/channels.scm"
        GIT_PULL_COMMAND = "git pull --rebase upstream"
    }
    stages {
        stage("Pulling from upstream Git") {
            steps {
                dir(LOCAL_WORKTREE) {
                    sh GIT_PULL_COMMAND
                }
                dir(MASTER_WORKTREE) {
                    sh GIT_PULL_COMMAND
                }
            }
        }
        stage("Cloning from local Git") {
            steps {
                parallelGitClone url: "https://cgit.duckdns.org/git/guix/guix",
                branch: "wip-local",
                nodeLabels: ["guix", "guix nixbld", "guix vm"],
                dir: LOCAL_WORKTREE
            }
        }
        stage("Invoking guix pull") {
            steps {
                parallelSh cmd: GUIX_PULL_COMMAND,
                nodeLabels: ["guix", "guix nixbld", "guix vm"]
            }
        }
        stage("Invoking guix pull as root") {
            steps {
                parallelSh cmd: "sudo -i ${GUIX_PULL_COMMAND}",
                nodeLabels: ["guix", "guix nixbld", "guix vm"]
            }
        }
        stage("Building from Git") {
            steps {
                parallelSh cmd: BUILD_SCRIPT,
                nodeLabels: ["guix", "guix nixbld", "guix vm"],
                dir: LOCAL_WORKTREE
            }
        }
        stage("Building from master") {
            steps {
                dir(MASTER_WORKTREE) {
                    sh BUILD_SCRIPT
                }
            }
        }
        stage('Trigger dotfiles job') {
            steps {
                build job: "../../wigust/dotfiles/master"
            }
        }
    }
    post {
        always {
            sendNotifications currentBuild.result
        }
    }
}
