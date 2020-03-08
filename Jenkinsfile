pipeline {
  agent any
  stages {
    stage('Test') {
      steps {
        pwd
        ls ~/
        sh 'python -m unittest discover chapters/chapter_1/'
      }
    }
  }
}
