pipeline {
    agent {
        label "master"
    }
    parameters {
        choice(name: "ACTION", choices: ["version", "make", "pull", "describe"], description: "Choose an action.")
    }
    environment {
        GUIX_REPOSITORY_LOCAL = "/home/oleg/src/guix"
    }
    stages {
        stage("guix version") {
            when { expression { params.ACTION == "version" } }
            steps {
                sh "guix --version"
            }
        }
        stage("guix describe") {
            when { expression { params.ACTION == "describe" } }
            steps {
                sh "guix describe"
            }
        }
        stage("git clone guix") {
            when { expression { params.ACTION == "make" } }
            steps {
                parallelGitClone url: "https://cgit.duckdns.org/git/guix/guix", branch: "wip-local", nodeLabels: ["guix"], dir: GUIX_REPOSITORY_LOCAL
            }
        }
        stage("guix pull") {
            when { anyOf {
                    expression { params.ACTION == "make" }
                    expression { params.ACTION == "pull" }
            } }
            steps {
                parallelSh cmd: "guix pull --channels=${GUIX_REPOSITORY_LOCAL}/channels.scm", nodeLabels: ["guix"]
            }
        }
        stage("sudo guix pull") {
            when { anyOf {
                    expression { params.ACTION == "make" }
                    expression { params.ACTION == "pull" }
            } }
            steps {
                parallelSh cmd: "sudo -i guix pull --channels=${GUIX_REPOSITORY_LOCAL}/channels.scm", nodeLabels: ["guix"]
            }
        }
        stage("make guix") {
            when { expression { params.ACTION == "make" } }
            steps {
                parallelSh cmd: '''
                  #!/bin/sh
                  env GUIX_PACKAGE_PATH= guix environment --pure guix --ad-hoc help2man guile-sqlite3 guile-gcrypt -- sh -c "set -e -x; ./bootstrap; ./configure --localstatedir=/var --prefix=; make"
                ''', nodeLabels: ["guix"], dir: "/home/oleg/src/guix"
            }
        }
    }
}
