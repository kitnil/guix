String LOCAL_WORKTREE = "/home/oleg/src/guix"
String MASTER_WORKTREE = "${LOCAL_WORKTREE}-master"

List<String> build_command = [
    "set -e -x",
    "./bootstrap",
    "./configure --localstatedir=/var --prefix=",
    "make"
]
List<String> packages = ["help2man", "guile-sqlite3", "guile-gcrypt"]
String BUILD_SCRIPT = """
#!/bin/sh
guix environment --pure guix --ad-hoc ${packages.join(" ")} -- sh -c '${build_command.join("; ")}'
"""

String GIT_PULL_REMOTE = "upstream"
String GUIX_PULL_BRANCH = "wip-local"
String GUIX_CHANNELS_FILE = "${LOCAL_WORKTREE}/channels.scm"
String GUIX_PULL_COMMAND = "guix pull --branch=${GUIX_PULL_BRANCH} --channels=${GUIX_CHANNELS_FILE}"
String GIT_PULL_COMMAND = "git pull --rebase ${GIT_PULL_REMOTE}"
String GUIX_GIT_REPOSITORY = "https://cgit.duckdns.org/git/guix/guix"

List<String> node_labels = ["guix", "guix nixbld", "guix vm"]

pipeline {
    agent { label "master" }
    environment { GUIX_PACKAGE_PATH = "" }
    stages {
        stage("Pulling from upstream Git") {
            steps {
                dir(LOCAL_WORKTREE) { sh GIT_PULL_COMMAND }
                dir(MASTER_WORKTREE) { sh GIT_PULL_COMMAND }
            }
        }
        stage("Cloning from local Git") {
            steps {
                parallelGitClone url: GUIX_GIT_REPOSITORY,
                nodeLabels: node_labels, dir: LOCAL_WORKTREE,
                branch: GUIX_PULL_BRANCH
            }
        }
        stage("Invoking guix pull") {
            steps {
                parallelSh cmd: GUIX_PULL_COMMAND, nodeLabels: node_labels
            }
        }
        stage("Invoking guix pull as root") {
            steps {
                parallelSh cmd: "sudo -i ${GUIX_PULL_COMMAND}",
                nodeLabels: node_labels
            }
        }
        stage("Building from Git") {
            steps {
                parallelSh cmd: BUILD_SCRIPT,
                nodeLabels: node_labels, dir: LOCAL_WORKTREE
            }
        }
        stage("Building from master") {
            steps {
                dir(MASTER_WORKTREE) {
                    sh BUILD_SCRIPT
                }
            }
        }
        stage('Trigger jobs') {
            steps {
                build job: "../../wigust/dotfiles/master"
                build job: "../../nix/maintenance/wip-local"
            }
        }
    }
    post {
        always {
            sendNotifications currentBuild.result
        }
    }
}
