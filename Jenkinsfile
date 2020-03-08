pipeline {
  agent any
  stages {
    stage('Test') {
      steps {
        sh 'python -m unittest discover /chapters/chapter_1/'
      }
    }
  }
}
