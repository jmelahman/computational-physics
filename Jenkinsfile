pipeline {
  agents any
  stages {
    stage('Test') {
      steps {
        pip install -r requirements.txt
        pytest
      }
    }
  }
}
