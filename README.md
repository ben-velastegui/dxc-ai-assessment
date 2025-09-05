### Setup (Conda recommended)

# Create the environment from YAML
conda env create -f environment.yml
conda activate dxc-ai

### Setup (pip alternative)

# Create a virtualenv
python -m venv .venv
source .venv/bin/activate   # Windows: .venv\Scripts\activate

# Install requirements
pip install -r requirements.txt
