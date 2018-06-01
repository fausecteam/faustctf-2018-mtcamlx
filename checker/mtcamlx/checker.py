from ctf_gameserver.checker import BaseChecker
from ctf_gameserver.checker.constants import *

import pexpect
import random
import string
import os

class MtCamlXChecker(BaseChecker):
    def _gen_username(self):
        return ''.join(random.sample(string.ascii_letters, 20))


    def _gen_password(self):
        return ''.join(random.sample(string.ascii_letters, 20))


    def _spawn(self):
        self.logger.debug("Connecting to %s via telnet", self._ip)
        # replace localhost
        # random terminal size wrt fingerprinting
        env = os.environ
        env['TERM'] = 'xterm'
        child = pexpect.spawn('timeout -s KILL 90 telnet %s 7777' % self._ip,
                              env=env,
                              dimensions=(random.randint(300, 600),random.randint(300, 600)))
        self.logger.debug("telnet spawned")
        try:
            child.expect('MtCamlX')
        except pexpect.exceptions.EOF as e:
            self.logger.debug("Could not establish connection: %s", child.before)
            raise

        self.logger.debug("Connection established")
        return child


    def _finish(self, child):
        self.logger.debug("Closing connection")
        child.write(b'\x1b')
        try:
            while True:
                child.read_nonblocking(32 * 1024, 10)
        except pexpect.exceptions.EOF:
            pass


    def _register(self, child, username, password):
        idx = child.expect(['Login', 'Register'])
        if idx == 0:
            self.logger.debug("changing to register mode")
            child.sendcontrol('r')
            child.expect('Register')
        child.write(username)
        child.write('\t')
        child.write(password)
        child.write('\n')
        self.logger.debug("done writing register data")


    def _login(self, child, username, password):
        self.logger.debug("log in with username %s and password %s", username, password)
        idx = child.expect(['Login', 'Register'])
        if idx == 1:
            self.logger.debug("changing to login mode")
            child.sendcontrol('r')
            child.expect('Login')
        child.write(username)
        child.write('\t')
        child.write(password)
        child.write('\n')


    def _send_message(self, child, to, message):
        child.write('m')
        child.expect('New Message')
        child.write(to)
        child.write('\t')
        child.write(message)
        child.sendcontrol('w')


    def place_flag(self):
        try:
            theflag = self.get_flag(self.tick)

            receiver   = self._gen_username()
            sender     = self._gen_username()
            pwreceiver = self._gen_password()
            pwsender   = self._gen_password()

            self.logger.debug('Registering receiving user %s', receiver)
            try:
                child = self._spawn()
                self._register(child, receiver, pwreceiver)
            except pexpect.exceptions.TIMEOUT:
                self.logger.debug("Output so far: %s", child.before)
                return NOTWORKING
            except pexpect.exceptions.EOF:
                return NOTWORKING
            self._finish(child)

            self.logger.debug('Registering sending user %s', sender)
            try:
                child = self._spawn()
                self._register(child, sender, pwsender)
            except pexpect.exceptions.TIMEOUT:
                self.logger.debug("Output so far: %s", child.before)
                return NOTWORKING
            self._finish(child)

            self.store_blob('flagid_%03d' % self.tick, receiver.encode())
            self.store_blob('password_%03d' % self.tick, pwreceiver.encode())

            self.logger.debug('Logging in as sender %s to send message', sender)
            self.logger.debug('placing flag %s', theflag)

            try:
                child = self._spawn()
                self._login(child, sender, pwsender)
                try:
                    child.expect_exact("Welcome back %s" % sender, timeout=40)
                except pexpect.exceptions.EOF:
                    self.logger.debug("Output so far: %s", child.before)
                    return NOTWORKING

                self._send_message(child, receiver, theflag)

            except pexpect.exceptions.TIMEOUT:
                self.logger.debug("Output so far: %s", child.before)
                return NOTWORKING
            self._finish(child)

            return OK
        except pexpect.exceptions.TIMEOUT:
            self.logger.debug("Output so far: %s", child.before)
            return TIMEOUT


    def check_flag(self, tick):
        try:
            theflag = self.get_flag(tick)
            self.logger.debug('looking for flag %s', theflag)

            username = self.retrieve_blob('flagid_%03d' % tick).decode()
            password = self.retrieve_blob('password_%03d' % tick).decode()

            try:
                child = self._spawn()
                self._login(child, username, password)

                child.expect_exact("Welcome back %s" % username, timeout=40)
            except pexpect.exceptions.EOF:
                self.logger.debug("login failed")
                return NOTFOUND
            except pexpect.exceptions.TIMEOUT:
                self.logger.debug("login failed")
                return NOTWORKING
            except Exception:
                self.logger.exception("shit")
                raise

            self.logger.debug("searching flag")
            child.write('r')
            try:
                child.expect_exact(theflag, timeout=40)
            except pexpect.exceptions.TIMEOUT:
                self.logger.debug("Output so far: %s", child.before)
                return NOTFOUND

            self._finish(child)

            return OK
        except pexpect.exceptions.TIMEOUT:
            self.logger.debug("Output so far: %s", child.before)
            return TIMEOUT


    def check_service(self):
        try:
            username = self.retrieve_blob('flagid_%03d' % self.tick).decode()
            password = self.retrieve_blob('password_%03d' % self.tick).decode()

            if random.uniform(0, 1) > 0.5:
                self.logger.debug("checking for help screen")
                child = self._spawn()
                self._login(child, username, password)

                try:
                    child.expect_exact("Welcome back %s" % username, timeout=40)
                except pexpect.exceptions.TIMEOUT:
                    self.logger.debug("Output so far: %s", child.before)
                    return NOTWORKING

                child.write('?')
                try:
                    child.expect_exact("Refresh current values", timeout=40)
                except pexpect.exceptions.TIMEOUT:
                    self.logger.debug("Output so far: %s", child.before)
                    return NOTWORKING

            if random.uniform(0, 1) > 0.8:
                self.logger.debug("trying invalid password")
                try:
                    child = self._spawn()
                    self._login(child, username, self._gen_password())
                except pexpect.exceptions.TIMEOUT:
                    self.logger.debug("Output so far: %s", child.before)
                    return NOTWORKING

                try:
                    child.expect_exact("Welcome back %s" % username, timeout=40)
                    return NOTWORKING
                except pexpect.exceptions.TIMEOUT:
                    pass

                self._finish(child)

            if random.uniform(0, 1) > 0.1:
                self.logger.debug("sending some fishy strings")
                fishy = [ "`cat persistent.sqlite`"
                        , "A" * 100
                        , "\x90" * 42
                        , r"TX-3399-Purr-!TTTP\%JONE%501:-%mm4-%mm%--DW%P-Yf1Y-fwfY-yzSzP-iii%-Zkx%-%Fw%P-XXn6- 99w%-ptt%P-%w%%-qqqq-jPiXP-cccc-Dw0D-WICzP-c66c-W0TmP-TTTT-%NN0-%o42-7a-0P-xGGx-rrrx- aFOwP-pApA-N-w--B2H2PPPPPPPPPPPPPPPPPPPPPP"
                        ]
                fishychoice = random.choice(fishy)
                self.logger.debug("fishy string this time is %s", fishychoice)
                child = self._spawn()
                self._login(child, username, password)
                try:
                    child.expect_exact("Welcome back %s" % username, timeout=40)
                except pexpect.exceptions.EOF:
                    self.logger.debug("Output so far: %s", child.before)
                    return NOTWORKING

                self._send_message(child, username, fishychoice)

            return OK
        except pexpect.exceptions.TIMEOUT:
            self.logger.debug("Output so far: %s", child.before)
            return TIMEOUT
