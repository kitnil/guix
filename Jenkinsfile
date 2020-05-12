pipeline {
    agent { label "master" }
    environment { GUIX_PACKAGE_PATH = "" }
    stages {
        stage("Deploy") {
            steps {
                parallelCall (
                    nodeLabels: ["guix"],
                    procedure: { nodeLabels ->
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
    }
    post { always { sendNotifications currentBuild.result } }
}
