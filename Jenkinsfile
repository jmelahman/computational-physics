pipeline {
  agent any
  stages {
    stage('Test') {
      steps {
        sh 'python3 -m unittest discover /var/lib/jenkins/jobs/jmelahman/jobs/computational-physics/branches/master/workspace/chapters/chapter_1/'
      }
    }
  }
}
