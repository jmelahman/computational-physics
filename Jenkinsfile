pipeline {
  agent any
  stages {
    stage('Test') {
      steps {
        python -m unittest discover chapters/chapter_1/
      }
    }
  }
}
