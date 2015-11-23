CF=/opt/cloudfleet
CF_VAR=${CF}/var
CF_TMP=${CF_VAR}/tmp

CF_DATA=${CF}/data
CF_APPS=${CF}/apps

mkdir -p "$CF_APPS" || echo "Failed to create $CF_APPS" 
mkdir -p "$CF_TMP" || echo "Failed to create $CF_TMP" 

# TODO use set -e on invocation?
# split export up for easier parsing and ksh compatibility
export CF CF_VAR CF_DATA
