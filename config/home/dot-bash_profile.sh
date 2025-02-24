

# Add ~/.guix-profile if it exists
# .guix-profile -> /var/guix/profiles/per-user/logoraz/guix-profile
guix_profile="/home/logoraz/.guix-profile"
if [[ -L $guix_profile && -d $guix_profile ]]; then
    GUIX_PROFILE="/home/logoraz/.guix-profile"
    . "$GUIX_PROFILE/etc/profile"
fi
