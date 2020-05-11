String GIT_LOCAL_WORKTREE = "$Constants.homeDir/src/guix"
List<String> BUILD_PACKAGES = ["help2man", "guile-sqlite3", "guile-gcrypt"]
String BUILD_COMMAND = """
    guix environment --pure guix --ad-hoc ${BUILD_PACKAGES.join(' ')} \
      -- sh -c "(set -e -x; ./bootstrap; ./configure --localstatedir=/var --prefix=; make) \
        || (set -e -x; make clean-go; ./bootstrap; ./configure --localstatedir=/var --prefix=; make)"
"""

pipeline {
    agent { label "master" }
    environment { GUIX_PACKAGE_PATH = "" }
    triggers { cron("H 14 * * 1-5") }
    stages {
        stage("Fetch source") {
            steps {
                parallelGitClone (
                    nodeLabels: ["guix"],
                    url: Constants.gitGuixUrl,
                    branch: "wip-local",
                    dir: GIT_LOCAL_WORKTREE
                )
            }
        }
        stage("Build guix") {
            steps {
                parallelSh (
                    nodeLabels: ["guix"],
                    dir: GIT_LOCAL_WORKTREE,
                    cmd: BUILD_COMMAND
                )
            }
        }
        stage("Build guix-*") {
            agent { label "guixsd" }
            steps {
                dir("$Constants.homeDir/src/guix-master") {
                    sh "git pull --rebase upstream"
                    sh BUILD_COMMAND
                }
                dir("$Constants.homeDir/src/guix-wip-local-master") {
                    sh "git pull --rebase upstream"
                    sh BUILD_COMMAND
                }
            }
        }
        stage("Trigger dotfiles") {
            steps {
                catchError(buildResult: "SUCCESS", stageResult: "FAILURE") {
                    build job: "../../wigust/dotfiles/master"
                }
            }
        }
    }
    post { always { sendNotifications currentBuild.result } }
}
