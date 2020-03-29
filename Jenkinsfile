List<String> node_labels = ["guix", "guix nixbld", "guix vm"]

String LOCAL_WORKTREE = "/home/oleg/src/guix"
String MASTER_WORKTREE = "${LOCAL_WORKTREE}-master"
List<String> build_command = [
    "set -e -x",
    "./bootstrap",
    "./configure --localstatedir=/var --prefix=",
    "make"
]
List<String> packages = ["help2man", "guile-sqlite3", "guile-gcrypt"]
String BUILD_COMMAND = """sh -c '${build_command.join("; ")}'"""
String BUILD_SCRIPT = [
    "guix", "environment", "--pure", "guix", "--ad-hoc", packages.join(' '),
    "--", BUILD_COMMAND
].join(" ")


String GUIX_GIT_PULL_REMOTE = "upstream"
String GUIX_GIT_PULL_BRANCH = "wip-local"
String GUIX_GIT_PULL_COMMAND = "git pull --rebase ${GUIX_GIT_PULL_REMOTE}"
String GUIX_GIT_REPOSITORY = "https://cgit.duckdns.org/git/guix/guix"

pipeline {
    agent { label "master" }
    environment { GUIX_PACKAGE_PATH = "" }
    triggers {
        cron("H 14 * * 1-5")
    }
    parameters {
        booleanParam name: "INVOKE_GIT_PULL", defaultValue: true,
        description: 'Update Guix Git repository'
    }
    stages {
        stage("Invoking git clone") {
            steps {
                parallelGitClone url: GUIX_GIT_REPOSITORY,
                nodeLabels: node_labels, dir: LOCAL_WORKTREE,
                branch: GUIX_GIT_PULL_BRANCH
            }
        }
        stage("Invoking git pull") {
            when { expression { params.INVOKE_GIT_PULL } }
            steps {
                // dir(LOCAL_WORKTREE) { sh GUIX_GIT_PULL_COMMAND }
                dir(MASTER_WORKTREE) { sh GUIX_GIT_PULL_COMMAND }
            }
        }
        stage('Trigger jobs') {
            steps {
                catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
                    build job: "../../wigust/dotfiles/master"
                }
            }
        }
        stage("Build local worktree") {
            steps {
                parallelSh (cmd: BUILD_SCRIPT, nodeLabels: node_labels,
                            dir: LOCAL_WORKTREE)
            }
        }
        stage("Build master worktree") {
            steps {
                dir(MASTER_WORKTREE) {
                    sh BUILD_SCRIPT
                }
            }
        }
    }
    post {
        always {
            sendNotifications currentBuild.result
        }
    }
}
