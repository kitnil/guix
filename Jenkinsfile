def slackMessages = []

pipeline {
    agent { label "master" }
    environment { GUIX_PACKAGE_PATH = "" }
    options {
        disableConcurrentBuilds()
    }
    stages {
        stage("Deploy") {
            steps {
                parallelCall (
                    nodeLabels: ["guix"],
                    procedure: { nodeLabels ->
                        String repository = "$Constants.homeDir/src/guix"
                        gitFetch (
                            branch: GIT_BRANCH,
                            url: Constants.gitGuixUrl,
                            dir: repository
                        )
                        dir(repository) {
                            guix.build()
                            sh "git checkout -- po"
                        }
                        slackMessages += "Deployed to $Constants.homeDir/src/guix"
                    }
                )
            }
        }
        stage("Trigger") {
            when { branch "wip-local" }
            steps { build(job: "../../wigust/dotfiles/master") }
        }
    }
    post {
        always {
            sendSlackNotifications (
                buildStatus: currentBuild.result,
                threadMessages: slackMessages
            )
        }
    }
}
