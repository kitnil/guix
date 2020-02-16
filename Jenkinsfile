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

pipeline {
    agent { label "spb" }
    environment { GUIX_PACKAGE_PATH = "" }
    stages {
        stage("Build") {
            steps {
                sh BUILD_SCRIPT
            }
        }
    }
    post {
        always {
            sendNotifications currentBuild.result
        }
    }
}
