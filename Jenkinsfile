List<String> BUILD_PACKAGES = [
    "help2man",
    "guile-sqlite3",
    "guile-gcrypt"
]
List<String> BUILD_SCRIPTS = [
    'make --jobs=$(nproc)',
    '(set -e -x; ./bootstrap; ./configure --localstatedir=/var --prefix=; make --jobs=$(nproc))',
    '(set -e -x; make clean-go; ./bootstrap; ./configure --localstatedir=/var --prefix=; make --jobs=$(nproc))'
]
String BUILD_COMMAND = """
    guix environment --pure guix --ad-hoc ${BUILD_PACKAGES.join(' ')} \
      -- sh -c "${BUILD_SCRIPTS.join(' || ')}"
"""

pipeline {
    agent { label "master" }
    environment { GUIX_PACKAGE_PATH = "" }
    triggers { cron("H 14 * * 1-5") }
    stages {
        stage("Build") {
            steps {
                parallelCall (
                    nodeLabels: ["guix"],
                    procedure: {
                        gitFetch (
                            branch: "wip-local",
                            url: Constants.gitGuixUrl,
                            dir: "$Constants.homeDir/src/guix"
                        )
                        dir("$Constants.homeDir/src/guix") { sh BUILD_COMMAND }
                    }
                )
            }
        }
        stage("Trigger") {
            steps {
                catchError(buildResult: "SUCCESS", stageResult: "FAILURE") {
                    build job: "../../wigust/dotfiles/master"
                }
            }
        }
    }
    post { always { sendNotifications currentBuild.result } }
}
