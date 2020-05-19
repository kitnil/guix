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
                        guix.build(
                            branch: GIT_BRANCH,
                            dir: "$Constants.homeDir/src/guix"
                        )
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
